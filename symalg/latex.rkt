#lang racket/base

;; Converting symbolic algebraic expressions to LaTeX math strings.

(provide latex)

;; ---------------------------------
;; Import and implementation section

(require multimethod
         racket/string
         racket/match
         racket/list
         racket/bool
         "private/data.rkt"
         "private/util.rkt")

;; -----
;; latex
;; -----

(define-generic (latex e))

;; ---------
;; num-latex
;; ---------

(module+ test
  (require rackunit
           "simplify.rkt"
           "parse.rkt")

  (check-equal? (latex (make-num 3))
                "3")
  (check-equal? (latex (make-num 0))
                "0")
  )

(define-instance ((latex num) n)
  (number->string (num-val n)))

;; ----------
;; frac-latex
;; ----------

(module+ test
  (check-equal? (latex (make-frac 3 4))
                "\\frac{3}{4}")
  (check-equal? (latex (make-frac -3 4))
                "-\\frac{3}{4}")
  (check-equal? (latex (make-frac 3 -4))
                "-\\frac{3}{4}")
  )

(define-instance ((latex frac) f)
  (define num (frac-num f))
  (define denom (frac-denom f))
  (define sign
    (if (xor (> 0 num)
             (> 0 denom))
        "-"
        ""))
  (format "~a\\frac{~a}{~a}" sign (abs num) (abs denom)))

;; ---------
;; constant-latex
;; ---------

(module+ test
  (check-equal? (latex (make-constant 'pi))
                "\\pi")
  (check-equal? (latex (make-constant 'e))
                "e")
  (check-equal? (latex (make-mul (make-constant 'e) (make-constant 'pi)))
                "e \\pi")
  )

(define-instance ((latex constant) c)
  (match (constant-name c)
    ['pi "\\pi"]
    ['e  "e"]))

;; ---------
;; sym-latex
;; ---------

(module+ test
  (check-equal? (latex (make-sym 'x))
                "x")
  (check-equal? (latex (make-sym 'y_1))
                "y_1")
  )

(define-instance ((latex sym) s)
  (symbol->string (sym-val s)))

;; ---------
;; add-latex
;; ---------

(module+ test
  (check-equal? (latex (make-add (make-num 3) (make-sym 'x)))
                "3 + x")
  (check-equal? (latex (make-add (make-num 2) (make-sym 'x) (make-sym 'y)))
                "2 + x + y")
  (check-equal? (latex (make-add (make-num 3)
                                 (make-mul (make-num -1)
                                           (make-sym 'x))))
                "3 -x")
  (check-equal? (latex (make-add (make-num 3)
                                 (make-mul (make-frac -1 2)
                                           (make-sym 'x))))
                "3 -\\frac{1}{2} x")
  )

(define-instance ((latex add) a)
  (define addends
    (for/list ([addend (add-addends a)])
      (define l (latex addend))
      (cons (if (string-startswith l "-")
                " "
                " + ")
            l)))
  (apply string-append (cdr (flatten addends))))

;; ---------
;; mul-latex
;; ---------

(module+ test
  (check-equal? (latex (make-mul (make-num 3) (make-sym 'x)))
                "3 x")
  (check-equal? (latex (make-mul (make-num 2) (make-sym 'x) (make-sym 'y)))
                "2 x y")
  (check-equal? (latex (parse-sexpr '(* (+ 2 x) (+ 3 y))))
                "(2 + x) (3 + y)")
  (check-equal? (latex (make-mul (make-num -1) (make-sym 'x)))
                "-x")
  (check-equal? (latex (simplify (parse-sexpr '(/ 1 x))))
                "\\frac{1}{x}")
  (check-equal? (latex (simplify (parse-sexpr '(/ (* 3 a b) (* 4 x y)))))
                "\\frac{3}{4} \\frac{a b}{x y}")
  )

(define-instance ((latex mul) m)
  (define (format-* factors)
    (string-join
      (for/list ([factor factors])
        (parentize factor))
      " "))
  (define (format-/ f+ f-)
    (cond ((null? f-)
           (format-* f+))
          ((null? f+)
           (format-/ (list (make-num 1)) 
                     f-))
          ((and (> (length f+) 1)
                (or (frac? (car f+))
                    (num? (car f+))))
           (format "~a ~a" (latex (car f+))
                           (format-/ (cdr f+) f-)))
          (else
            (format "\\frac{~a}{~a}" (format-* f+) (format-* f-)))))
  (define factors (mul-factors m))
  (define factors-+ (filter (negate negative-exponent?) factors))
  (define factors-- (map abs-exponent
                         (filter negative-exponent? factors)))
  (if (and (not (null? factors-+))
           (equal? (car factors-+) (make-num -1)))
      (string-append "-" 
                     (format-/ (cdr factors-+) factors--))
      (format-/ factors-+ factors--)))

;; -----------
;; power-latex
;; -----------

(module+ test
  (check-equal? (latex (make-power (make-num 3) (make-sym 'x)))
                "3^{x}")
  (check-equal? (latex (parse-sexpr '(expt (+ x 3) 4)))
                "(x + 3)^{4}")
  (check-equal? (latex (make-power (make-sym 'x) (make-num -1)))
                "\\frac{1}{x}")
  )

(define-instance ((latex power) p)
  (if (negative-exponent? p)
      (latex (make-mul p))
      (format "~a^{~a}" 
              (parentize (power-base p))
              (latex (power-exponent p)))))

;; ----------
;; logn-latex
;; ----------

(module+ test
  (check-equal? (latex (make-logn (make-num 3) (make-sym 'x)))
                "\\log_{x} 3")
  (check-equal? (latex (make-logn (make-num 3) (make-num (exp 1))))
                "\\ln 3")
  )

(define-instance ((latex logn) l)
  (define s/n (latex (logn-n l)))
  (define base (logn-base l))
  (cond ((equal? base (make-num (exp 1)))
         (format "\\ln ~a" s/n))
        (else
          (format "\\log_{~a} ~a" (latex base) s/n))))

;; ----------
;; cos-latex
;; ----------

(module+ test
  (check-equal? (latex (make-cos (make-num 3)))
                "\\cos(3)")
  (check-equal? (latex (make-cos (make-sym 'x)))
                "\\cos(x)")
  )

(define-instance ((latex cos_) c)
  (format "\\cos(~a)" (latex (cos_-n c))))

;; ----------
;; sin-latex
;; ----------

(module+ test
  (check-equal? (latex (make-sin (make-num 3)))
                "\\sin(3)")
  (check-equal? (latex (make-sin (make-sym 'x)))
                "\\sin(x)")
  )

(define-instance ((latex sin_) s)
  (format "\\sin(~a)" (latex (sin_-n s))))

;; ----------
;; tan-latex
;; ----------

(module+ test
  (check-equal? (latex (make-tan (make-num 3)))
                "\\tan(3)")
  (check-equal? (latex (make-tan (make-sym 'x)))
                "\\tan(x)")
  )

(define-instance ((latex tan_) s)
  (format "\\tan(~a)" (latex (tan_-n s))))

;; -------------------
;; polynomial/si-latex
;; -------------------

(module+ test
  (check-equal? (latex (make-polynomial/si 'x '(0 1 2)))
                "2 x^{2} +  x")
  (check-equal? (latex (make-polynomial/si 'x '(3 3 3 3)))
                "3 x^{3} + 3 x^{2} + 3 x + 3 ")
  (check-equal? (latex (make-polynomial/si 'y '(9 0 0 0 0 1)))
                " y^{5} + 9 ")
  (check-equal? (latex (make-polynomial/si 'y '()))
                "0")
  (check-equal? (latex (make-polynomial/si 'y '(4)))
                "4 ")
  (check-equal? (latex (make-polynomial/si 'y '(0 0 1)))
                " y^{2}")
  )

(define-instance ((latex polynomial/si) p)
  (define indet (symbol->string (polynomial/si-indet p)))
  (define parts
    (for/list ([c (polynomial/si-coeffs p)]
               [e (in-naturals)]
               #:when (> c 0))
      (string-append (if (> c 1)
                         (number->string c)
                         "")
                     " "
                     (if (> e 0)
                         indet
                         "")
                     (if (> e 1)
                         (format "^{~a}" e)
                         ""))))
  (match (length parts)
    [0 "0"]
    [1 (car parts)]
    [_ (string-join (reverse parts) " + ")]))


; -----
; atom? 
; -----

(module+ test
  (check-true  (atom? (make-num 3)))
  (check-true  (atom? (make-sym 'x)))
  (check-true  (atom? (make-constant 'pi)))
  (check-true  (atom? (parse-sexpr '(expt x 3))))
  (check-false (atom? (parse-sexpr '(* x 3))))
  (check-false (atom? (parse-sexpr '(+ x 3))))
  (check-true  (atom? (parse-sexpr '(ln 3))))
  (check-true  (atom? (parse-sexpr '(cos 3))))
  (check-true  (atom? (parse-sexpr '(sin 3))))
  (check-true  (atom? (parse-sexpr '(tan 3))))
  )

(define (atom? e)
  (or (num? e)
      (sym? e)
      (frac? e)
      (cos_? e)
      (sin_? e)
      (tan_? e)
      (logn? e)
      (constant? e)
      (power? e)))

; ---------
; parentize
; ---------

(module+ test
  (check-equal? (parentize (parse-sexpr '(+ x 3)))
                "(x + 3)")
  (check-equal? (parentize (make-num 3))
                "3")
  )

(define (parentize e)
  (if (atom? e)
      (latex e)
      (format "(~a)" (latex e))))

; -----------------
; negative-exponent 
; -----------------

(module+ test
  (check-true  (negative-exponent? (parse-sexpr '(expt x -1))))
  (check-false (negative-exponent? (parse-sexpr '(expt x 2))))
  (check-false (negative-exponent? (parse-sexpr '(expt x y))))
  )

(define (negative-exponent? p)
  (and (power? p)
       (num? (power-exponent p))
       (negative? (num-val (power-exponent p)))))

; ------------
; abs-exponent
; ------------

(module+ test
  (check-equal? (abs-exponent (parse-sexpr '(expt x -2)))
                (parse-sexpr '(expt x 2)))
  (check-equal? (abs-exponent (parse-sexpr '(expt x 2)))
                (parse-sexpr '(expt x 2)))
  (check-equal? (abs-exponent (parse-sexpr '(expt x -1)))
                (parse-sexpr 'x))
  )

(define (abs-exponent p)
  (define exponent-val (abs (num-val (power-exponent p))))
  (define base (power-base p))
  (if (= 1 exponent-val)
      base
      (make-power base (make-num exponent-val))))
