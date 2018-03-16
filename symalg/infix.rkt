#lang racket/base

;; Converting symbolic algebraic expressions to infix/i expressions.

(provide infix)

;; ---------------------------------
;; Import and implementation section

(require multimethod
         racket/string
         "private/data.rkt")

;; -----
;; infix/i
;; -----

(define (infix e [resolve #f])
  (infix/i e resolve))

(define-generic (infix/i e))

;; ---------
;; num-infix/i
;; ---------

(module+ test
  (require rackunit
           racket/math)

  (check-equal? (infix (make-num 3))
                "3")
  (check-equal? (infix (make-num 0))
                "0")
  )

(define-instance ((infix/i num) n resolve)
  (number->string (num-val n)))

;; ----------
;; frac-infix/i
;; ----------

(module+ test
  (check-equal? (infix (make-frac 3 4))
                "3/4")
  )

(define-instance ((infix/i frac) f resolve)
  (format "~a/~a" (frac-num f) (frac-denom f)))

;; --------------
;; constant-infix/i
;; --------------

(module+ test
  (check-equal? (infix (make-constant 'pi))
                "pi")
  (check-equal? (infix (make-constant 'pi) #t)
                (format "~a" pi))
  (check-equal? (infix (make-constant 'e))
                "e")
  (check-equal? (infix (make-constant 'e) #t)
                (format "~a" (exp 1)))
  )

(define-instance ((infix/i constant) c resolve)
  (format "~a" (if resolve
                   (constant-value c)
                   (constant-name c))))

;; ---------
;; sym-infix/i
;; ---------

(module+ test
  (check-equal? (infix (make-sym 'x))
                "x")
  )

(define-instance ((infix/i sym) s resolve)
  (symbol->string (sym-val s)))

;; ---------
;; add-infix/i
;; ---------

(module+ test
  (check-equal? (infix (make-add (make-num 3) (make-num 4)))
                "3 + 4")
  (check-equal? (infix (make-add (make-num 3) (make-sym 'x) (make-num 4)))
                "3 + x + 4")
  )

(define-instance ((infix/i add) a resolve)
  (string-join
    (for/list ([e (add-addends a)])
      (infix/i e resolve))
    " + "))

;; ---------
;; mul-infix/i
;; ---------

(module+ test
  (check-equal? (infix (make-mul (make-num 3) (make-num 4)))
                "3 * 4")
  (check-equal? (infix (make-mul (make-num 3) (make-sym 'x) (make-num 4)))
                "3 * x * 4")
  )

(define-instance ((infix/i mul) m resolve)
  (string-join
    (for/list ([e (mul-factors m)])
      (infix/i e resolve))
    " * "))

;; -----------
;; power-infix/i
;; -----------

(module+ test
  (check-equal? (infix (make-power (make-num 3) (make-num 4)))
                "(3)^(4)")
  (check-equal? (infix (make-power (make-num 3) (make-sym 'x)))
                "(3)^(x)")
  )

(define-instance ((infix/i power) p resolve)
  (format "(~a)^(~a)" (infix/i (power-base p) resolve)
                      (infix/i (power-exponent p) resolve)))

;; ----------
;; logn-infix/i
;; ----------

(module+ test
  (check-equal? (infix (make-logn (make-num 3) (make-num 4)))
                "logn(3, 4)")
  (check-equal? (infix (make-logn (make-num 3) (make-sym 'x)))
                "logn(3, x)")
  (check-equal? (infix (make-logn (make-num 3) (make-num (exp 1))))
                "ln(3)")
  )

(define-instance ((infix/i logn) l resolve)
  (define s/n (infix/i (logn-n l) resolve))
  (define base (logn-base l))
  (cond ((equal? base (make-num (exp 1)))
         (format "ln(~a)" s/n))
        (else
          (format "logn(~a, ~a)" s/n (infix/i base resolve)))))

;; -----------
;; cos-infix/i
;; -----------

(module+ test
  (check-equal? (infix (make-cos (make-num 3)))
                "cos(3)")
  (check-equal? (infix (make-cos (make-sym 'x)))
                "cos(x)")
  )

(define-instance ((infix/i cos_) c resolve)
  (define s/n (infix/i (cos_-n c) resolve))
  (format "cos(~a)" s/n))

;; -----------
;; sin-infix/i
;; -----------

(module+ test
  (check-equal? (infix (make-sin (make-num 3)))
                "sin(3)")
  (check-equal? (infix (make-sin (make-sym 'x)))
                "sin(x)")
  )

(define-instance ((infix/i sin_) c resolve)
  (define s/n (infix/i (sin_-n c) resolve))
  (format "sin(~a)" s/n))

;; -----------
;; tan-infix/i
;; -----------

(module+ test
  (check-equal? (infix (make-tan (make-num 3)))
                "tan(3)")
  (check-equal? (infix (make-tan (make-sym 'x)))
                "tan(x)")
  )

(define-instance ((infix/i tan_) c resolve)
  (define s/n (infix/i (tan_-n c) resolve))
  (format "tan(~a)" s/n))
