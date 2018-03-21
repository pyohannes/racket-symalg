#lang racket/base

;; Converting symbolic algebraic expressions to s-expressions.

(provide sexpr)

;; ---------------------------------
;; Import and implementation section

(require multimethod
         racket/match
         "private/data.rkt")

;; -----
;; sexpr
;; -----

(define-generic (sexpr e))

;; ---------
;; num-sexpr
;; ---------

(module+ test
  (require rackunit)

  (check-equal? (sexpr (make-num 3))
                '3)
  (check-equal? (sexpr (make-num 0))
                '0)
  )

(define-instance ((sexpr num) n)
  (num-val n))

;; ----------
;; frac-sexpr
;; ----------

(module+ test
  (check-equal? (sexpr (make-frac 3 4))
                '(/ 3 4))
  )

(define-instance ((sexpr frac) f)
  (list '/
        (frac-num f)
        (frac-denom f)))

;; --------------
;; constant-sexpr
;; --------------

(module+ test
  (check-equal? (sexpr (make-constant 'pi))
                'pi)
  (check-equal? (sexpr (make-constant 'e))
                'e)
  )

(define-instance ((sexpr constant) c)
  (constant-name c))

;; ---------
;; sym-sexpr
;; ---------

(module+ test
  (check-equal? (sexpr (make-sym 'x))
                'x)
  )

(define-instance ((sexpr sym) s)
  (sym-val s))

;; ---------
;; add-sexpr
;; ---------

(module+ test
  (check-equal? (sexpr (make-add (make-num 3) (make-num 4)))
                '(+ 3 4))
  (check-equal? (sexpr (make-add (make-num 3) (make-sym 'x) (make-num 4)))
                '(+ 3 x 4))
  )

(define-instance ((sexpr add) a)
  (cons '+
        (for/list ([addend (add-addends a)])
          (sexpr addend))))

;; ---------
;; mul-sexpr
;; ---------

(module+ test
  (check-equal? (sexpr (make-mul (make-num 3) (make-num 4)))
                '(* 3 4))
  (check-equal? (sexpr (make-mul (make-num 3) (make-sym 'x) (make-num 4)))
                '(* 3 x 4))
  )

(define-instance ((sexpr mul) m)
  (cons '*
        (for/list ([factor (mul-factors m)])
          (sexpr factor))))

;; -----------
;; power-sexpr
;; -----------

(module+ test
  (check-equal? (sexpr (make-power (make-num 3) (make-num 4)))
                '(expt 3 4))
  (check-equal? (sexpr (make-power (make-num 3) (make-sym 'x)))
                '(expt 3 x))
  )

(define-instance ((sexpr power) p)
  (list 'expt
        (sexpr (power-base p))
        (sexpr (power-exponent p))))

;; ----------
;; logn-sexpr
;; ----------

(module+ test
  (check-equal? (sexpr (make-logn (make-num 3) (make-num 4)))
                '(log 3 4))
  (check-equal? (sexpr (make-logn (make-num 3) (make-sym 'x)))
                '(log 3 x))
  (check-equal? (sexpr (make-logn (make-num 3) (make-num (exp 1))))
                '(ln 3))
  )

(define-instance ((sexpr logn) l)
  (define s/n (sexpr (logn-n l)))
  (define base (logn-base l))
  (cond ((equal? base (make-num (exp 1)))
         (list 'ln s/n))
        (else
          (list 'log s/n (sexpr base)))))

;; ----------
;; cos-sexpr
;; ----------

(module+ test
  (check-equal? (sexpr (make-cos (make-num 3)))
                '(cos 3))
  (check-equal? (sexpr (make-cos (make-sym 'x)))
                '(cos x))
  )

(define-instance ((sexpr cos_) c)
  (list 'cos (sexpr (cos_-n c))))

;; ----------
;; sin-sexpr
;; ----------

(module+ test
  (check-equal? (sexpr (make-sin (make-num 3)))
                '(sin 3))
  (check-equal? (sexpr (make-sin (make-sym 'x)))
                '(sin x))
  )

(define-instance ((sexpr sin_) c)
  (list 'sin (sexpr (sin_-n c))))

;; ----------
;; tan-sexpr
;; ----------

(module+ test
  (check-equal? (sexpr (make-tan (make-num 3)))
                '(tan 3))
  (check-equal? (sexpr (make-tan (make-sym 'x)))
                '(tan x))
  )

(define-instance ((sexpr tan_) c)
  (list 'tan (sexpr (tan_-n c))))

;; -------------------
;; polynomial/si-sexpr
;; -------------------

(module+ test
  (check-equal? (sexpr (make-polynomial/si 'x '(0 1 2)))
                '(+ (* 2 (expt x 2)) x))
  (check-equal? (sexpr (make-polynomial/si 'x '(3 3 3 3)))
                '(+ (* 3 (expt x 3)) (* 3 (expt x 2)) (* 3 x) 3))
  (check-equal? (sexpr (make-polynomial/si 'y '(9 0 0 0 0 1)))
                '(+ (expt y 5) 9))
  (check-equal? (sexpr (make-polynomial/si 'y '()))
                0)
  (check-equal? (sexpr (make-polynomial/si 'y '(4)))
                4)
  (check-equal? (sexpr (make-polynomial/si 'y '(0 0 1)))
                '(expt y 2))
  )

(define-instance ((sexpr polynomial/si) p)
  (define indet (polynomial/si-indet p))
  (define parts
    (for/list ([c (polynomial/si-coeffs p)]
               [e (in-naturals)]
               #:when (> c 0))
      (cond ((and (= c 1) (> e 1))
             (list 'expt indet e))
            ((and (= c 1) (= e 1))
             indet)
            ((= e 1)
             (list '* c indet))
            ((= e 0)
             c)
            (else
              `(* ,c (expt ,indet ,e))))))
  (match (length parts)
    [0 0]
    [1 (car parts)]
    [_ (cons '+ (reverse parts))]))

