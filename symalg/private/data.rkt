#lang racket/base

;; Definition of data structures.

(provide
  make-num make-sym make-add make-mul make-frac make-power make-logn 
  make-constant make-polynomial/si make-cos make-sin make-tan
  (struct-out num)
  (struct-out frac)
  (struct-out constant)
  (struct-out sym)
  (struct-out add)
  (struct-out mul)
  (struct-out power)
  (struct-out logn)
  (struct-out polynomial/si)
  (struct-out cos_)
  (struct-out sin_)
  (struct-out tan_)
  )

;; ---------------------------------
;; Import and implementation section

(require multimethod
         racket/math
         "util.rkt")

;; --------
;; make-num
;; --------

(module+ test
  (require rackunit)

  (check-equal? (make-num 3)
                (num 3))
  )

(define (make-num n)
  (num n))

;; ---
;; num
;; ---

(struct num (val))

;; -------------
;; make-constant
;; -------------

(module+ test
  (check-equal? (make-constant 'e)
                (constant 'e (exp 1)))
  (check-equal? (make-constant 'pi)
                (constant 'pi pi))
  (check-exn
    exn:fail?
    (lambda () (make-constant 'x)))
  (check-exn
    exn:fail?
    (lambda () (make-constant 1)))
  )

(define known-constants
  (hash 'e  (exp 1)
        'pi pi))

(define (make-constant c)
  (constant c (hash-ref known-constants c)))

;; --------
;; constant
;; --------

(struct constant (name value))

;; --------
;; make-sym
;; --------

(module+ test
  (check-equal? (make-sym 'x)
                (sym 'x))
  (check-exn
    exn:fail?
    (lambda () (make-sym 'abc)))
  (check-exn
    exn:fail?
    (lambda () (make-sym 1)))
  )

(define (make-sym s)
  (if (regexp-match #rx"^[a-zA-Z](_[a-zA-Z0-9]+)*$" (symbol->string s))
      (sym s)
      (error "Not a valid variable identifier: " s)))

;; ---
;; sym
;; ---

(struct sym (val))

;; --------
;; make-add
;; --------

(module+ test
  (check-equal? (make-add (make-num 3) (make-num 4))
                (add (list (make-num 3) (make-num 4))))
  )

(define (make-add n1 . rest)
  (add (cons n1 rest)))

;; ---
;; add
;; ---

(struct add (addends))

;; ---------
;; make-frac
;; ---------

(module+ test
  (check-equal? (make-frac 3 4)
                (frac 3 4))
  )

(define (make-frac n d)
  (frac n d))

;; ----
;; frac
;; ----

(struct frac (num denom))

;; --------
;; make-mul
;; --------

(module+ test
  (check-equal? (make-mul (make-num 3) (make-num 4))
                (mul (list (make-num 3) (make-num 4))))
  )

(define (make-mul n1 . rest)
  (mul (cons n1 rest)))

;; ---
;; mul
;; ---

(struct mul (factors))

;; ----------
;; make-power
;; ----------

(module+ test
  (check-equal? (make-power (make-num 3) (make-num 4))
                (power (make-num 3) (make-num 4)))
  )

(define (make-power n1 n2)
  (power n1 n2))

;; -----
;; power
;; -----

(struct power (base exponent))

;; ---------
;; make-logn
;; ---------

(module+ test
  (check-equal? (make-logn (make-num 3) (make-num 4))
                (logn (make-num 3) (make-num 4)))
  )

(define (make-logn n1 n2)
  (logn n1 n2))

;; ----
;; logn
;; ----

(struct logn (n base))

;; --------
;; make-cos
;; --------

(module+ test
  (check-equal? (make-cos (make-num 3))
                (cos_ (make-num 3)))
  (check-equal? (make-cos (make-sym 'x))
                (cos_ (make-sym 'x)))
  )

(define (make-cos n1)
  (cos_ n1))

;; ----
;; cos_
;; ----

(struct cos_ (n))

;; --------
;; make-sin
;; --------

(module+ test
  (check-equal? (make-sin (make-num 3))
                (sin_ (make-num 3)))
  (check-equal? (make-sin (make-sym 'x))
                (sin_ (make-sym 'x)))
  )

(define (make-sin n1)
  (sin_ n1))

;; ----
;; sin_
;; ----

(struct sin_ (n))

;; --------
;; make-tan
;; --------

(module+ test
  (check-equal? (make-tan (make-num 3))
                (tan_ (make-num 3)))
  (check-equal? (make-tan (make-sym 'x))
                (tan_ (make-sym 'x)))
  )

(define (make-tan n1)
  (tan_ n1))

;; ----
;; tan_
;; ----

(struct tan_ (n))

;; ------------------
;; make-polynomial/si
;; ------------------

(module+ test
  (require rackunit)

  (check-equal? (make-polynomial/si 'x '(9 0 1))
                (polynomial/si 'x '(9 0 1)))
  (check-equal? (make-polynomial/si 'x '(9 0 1 0))
                (polynomial/si 'x '(9 0 1)))
  )

(define (make-polynomial/si indet coeffs)
  (polynomial/si indet 
                 (list-trim-right coeffs 0)))

;; -------------
;; polynomial/si
;; -------------

(struct polynomial/si (indet coeffs))

