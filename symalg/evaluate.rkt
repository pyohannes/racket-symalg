#lang racket/base

;; Evaluation of expressions.

(provide evaluate)

;; ---------------------------------
;; Import and implementation section

(require multimethod
         "private/data.rkt")

;; --------
;; evaluate
;; --------

(define-generic (evaluate e))

;; ------------
;; num-evaluate
;; ------------

(module+ test
  (require rackunit
           racket/math)

  (check-equal? ((evaluate (make-num 3))
                 9)
                3)
  (check-equal? ((evaluate (make-num 0))
                 3)
                0)
  )

(define-instance ((evaluate num) n)
  (define (_ . rest)
    (num-val n))
  _)

;; -------------
;; frac-evaluate
;; -------------

(module+ test
  (check-equal? ((evaluate (make-frac 3 3))
                 9)
                1)
  (check-equal? ((evaluate (make-frac 3 4))
                 3)
                (/ 3 4))
  )

(define-instance ((evaluate frac) f)
  (define (_ . rest)
    (/ (frac-num f)
       (frac-denom f)))
  _)

;; -----------------
;; constant-evaluate
;; -----------------

(module+ test
  (check-equal? ((evaluate (make-constant 'pi))
                 9)
                pi)
  (check-equal? ((evaluate (make-constant 'e))
                 3)
                (exp 1))
  )

(define-instance ((evaluate constant) c)
  (define (_ . rest)
    (constant-value c))
  _)

;; ------------
;; sym-evaluate
;; ------------

(module+ test
  (check-equal? ((evaluate (make-sym 'x))
                 9)
                9)
  (check-equal? ((evaluate (make-sym 'y))
                 3)
                3)
  )

(define-instance ((evaluate sym) n)
  (define (_ . rest)
    (car rest))
  _)

;; ------------
;; add-evaluate
;; ------------

(module+ test
  (check-equal? ((evaluate (make-add (make-num 3) (make-sym 'x)))
                 9)
                12)
  (check-equal? ((evaluate (make-add (make-num 3) (make-sym 'x) (make-num 4)))
                 9)
                16)
  )

(define-instance ((evaluate add) a)
  (define (_ . rest)
    (for/sum ([addend (add-addends a)])
      (apply (evaluate addend) rest)))
  _)

;; ------------
;; mul-evaluate
;; ------------

(module+ test
  (check-equal? ((evaluate (make-mul (make-num 3) (make-sym 'x)))
                 9)
                27)
  (check-equal? ((evaluate (make-mul (make-num 3) (make-sym 'x) (make-num 4)))
                 9)
                108)
  )

(define-instance ((evaluate mul) m)
  (define (_ . rest)
    (for/product ([factor (mul-factors m)])
      (apply (evaluate factor) rest)))
  _)

;; --------------
;; power-evaluate
;; --------------

(module+ test
  (check-equal? ((evaluate (make-power (make-num 2) (make-sym 'x)))
                 4)
                16)
  (check-equal? ((evaluate (make-power (make-sym 'x) (make-num 3)))
                 3)
                27)
  )

(define-instance ((evaluate power) p)
  (define (_ . rest)
    (expt (apply (evaluate (power-base p)) rest)
          (apply (evaluate (power-exponent p)) rest)))
  _)

;; --------------
;; logn-evaluate
;; --------------

(module+ test
  (check-equal? ((evaluate (make-logn (make-num 2) (make-sym 'x)))
                 4)
                0.5)
  (check-equal? ((evaluate (make-logn (make-sym 'x) (make-num 3)))
                 3)
                1.0)
  )

(define-instance ((evaluate logn) l)
  (define (_ . rest)
    (log (apply (evaluate (logn-n l)) rest)
         (apply (evaluate (logn-base l)) rest)))
  _)

;; --------------
;; cos-evaluate
;; --------------

(module+ test
  (check-equal? ((evaluate (make-cos (make-num 2)))
                 4)
                (cos 2))
  (check-equal? ((evaluate (make-cos (make-sym 'x)))
                 3)
                (cos 3))
  )

(define-instance ((evaluate cos_) c)
  (define (_ . rest)
    (cos (apply (evaluate (cos_-n c)) rest)))
  _)

;; --------------
;; sin-evaluate
;; --------------

(module+ test
  (check-equal? ((evaluate (make-sin (make-num 2)))
                 4)
                (sin 2))
  (check-equal? ((evaluate (make-sin (make-sym 'x)))
                 3)
                (sin 3))
  )

(define-instance ((evaluate sin_) c)
  (define (_ . rest)
    (sin (apply (evaluate (sin_-n c)) rest)))
  _)

;; --------------
;; tan-evaluate
;; --------------

(module+ test
  (check-equal? ((evaluate (make-tan (make-num 2)))
                 4)
                (tan 2))
  (check-equal? ((evaluate (make-tan (make-sym 'x)))
                 3)
                (tan 3))
  )

(define-instance ((evaluate tan_) c)
  (define (_ . rest)
    (tan (apply (evaluate (tan_-n c)) rest)))
  _)

;; ----------------------
;; polynomial/si-evaluate
;; ----------------------

(module+ test
  (check-equal? ((evaluate (make-polynomial/si 'x '(0 1 2)))
                 9)
                171)
  (check-equal? ((evaluate (make-polynomial/si 'x '(3 3 3 3)))
                 3)
                120)
  (check-equal? ((evaluate (make-polynomial/si 'x '()))
                 3)
                0)
  )

(define-instance ((evaluate polynomial/si) p)
  (define (_ . rest)
    (define x (car rest))
    (for/sum ([c (polynomial/si-coeffs p)]
              [e (in-naturals)])
      (* c (expt x e))))
  _)

