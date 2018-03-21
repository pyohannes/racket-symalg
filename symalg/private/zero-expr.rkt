#lang racket/base

;; Testing if an expression is zero.

(provide zero-expr?)

;; ---------------------------------
;; Import and implementation section

(require multimethod
         "data.rkt")

;; -----
;; zero-expr?
;; -----

(define-generic (zero-expr? e))

;; ---------
;; num-zero-expr?
;; ---------

(module+ test
  (require rackunit)

  (check-true  (zero-expr? (make-num 0)))
  (check-false (zero-expr? (make-num 9)))
  )

(define-instance ((zero-expr? num) n)
  (= (num-val n) 0))

;; ---------
;; sym-zero-expr?
;; ---------

(module+ test
  (check-false (zero-expr? (make-sym 'x)))
  )

(define-instance ((zero-expr? sym) s)
  #f)

;; ----------
;; frac-zero-expr?
;; ----------

(module+ test
  (check-true  (zero-expr? (make-frac 0 3)))
  (check-false (zero-expr? (make-frac 1 3)))
  )

(define-instance ((zero-expr? frac) f)
  (= (frac-num f) 0))

;; -------------------
;; constant-zero-expr?
;; -------------------

(module+ test
  (check-false (zero-expr? (make-constant 'pi)))
  (check-false (zero-expr? (make-constant 'e)))
  )

(define-instance ((zero-expr? constant) c)
  #f)

;; ---------
;; add-zero-expr?
;; ---------

(module+ test
  (check-false (zero-expr? (make-add (make-num 3) (make-num 4))))
  )

(define-instance ((zero-expr? add) a)
  #f)

;; ---------
;; mul-zero-expr?
;; ---------

(module+ test
  (check-true  (zero-expr? (make-mul (make-num 1) (make-num 0))))
  (check-false (zero-expr? (make-mul (make-num 3) (make-num 4))))
  )

(define-instance ((zero-expr? mul) m)
  (for/first ([f (mul-factors m)]
              #:when (zero-expr? f))
    #t))

;; -----------
;; power-zero-expr?
;; -----------

(module+ test
  (check-true  (zero-expr? (make-power (make-num 0) (make-num 3))))
  (check-false (zero-expr? (make-power (make-num 3) (make-num 0))))
  )

(define-instance ((zero-expr? power) p)
  (zero-expr? (power-base p)))

;; ----------
;; logn-zero-expr?
;; ----------

(module+ test
  (check-false (zero-expr? (make-logn (make-num 2) (make-num 3))))
  )

(define-instance ((zero-expr? logn) l)
  #f)

;; --------------
;; cos-zero-expr?
;; --------------

(module+ test
  (check-false (zero-expr? (make-cos (make-sym 'x))))
  )

(define-instance ((zero-expr? cos_) c)
  #f)

;; --------------
;; sin-zero-expr?
;; --------------

(module+ test
  (check-false (zero-expr? (make-sin (make-sym 'x))))
  )

(define-instance ((zero-expr? sin_) c)
  #f)

;; --------------
;; tan-zero-expr?
;; --------------

(module+ test
  (check-false (zero-expr? (make-tan (make-sym 'x))))
  )

(define-instance ((zero-expr? tan_) c)
  #f)

;; -------------------
;; polynomial/si-zero-expr?
;; -------------------

(module+ test
  (check-false (zero-expr? (make-polynomial/si 'x '(1 2 3))))
  (check-true  (zero-expr? (make-polynomial/si 'x '())))
  (check-true  (zero-expr? (make-polynomial/si 'x '(0))))
  (check-true  (zero-expr? (make-polynomial/si 'x '(0 0 0 0))))
  )

(define-instance ((zero-expr? polynomial/si) p)
  (define coeffs (polynomial/si-coeffs p))
  (or (= 0 (length coeffs))
      (andmap zero? coeffs)))
