#lang racket/base

;; Return elements of an expression tree in pre-order.

(provide preorder)

;; ---------------------------------
;; Import and implementation section

(require multimethod
         "data.rkt")

;; --------
;; preorder
;; --------

(define-generic (preorder e))

;; ------------
;; num-preorder
;; ------------

(module+ test
  (require rackunit)

  (check-equal? (preorder (make-num 3))
                (list (make-num 3)))
  )

(define-instance ((preorder num) n)
  (list n))

;; -------------
;; frac-preorder
;; -------------

(module+ test
  (check-equal? (preorder (make-frac 3 4))
                (list (make-frac 3 4)))
  )

(define-instance ((preorder frac) f)
  (list f))

;; ------------
;; sym-preorder
;; ------------

(module+ test
  (check-equal? (preorder (make-sym 'x))
                (list (make-sym 'x)))
  )

(define-instance ((preorder sym) s)
  (list s))

;; -----------------
;; constant-preorder
;; -----------------

(module+ test
  (check-equal? (preorder (make-constant 'pi))
                (list (make-constant 'pi)))
  )

(define-instance ((preorder constant) c)
  (list c))

;; ------------
;; add-preorder
;; ------------

(module+ test
  (check-equal? (preorder (make-add (make-num 3) (make-sym 'x)))
                (list (make-add (make-num 3) (make-sym 'x))
                      (make-num 3)
                      (make-sym 'x)))
  (check-equal? (preorder (make-add (make-num 2) (make-sym 'x) (make-sym 'y)))
                (list (make-add (make-num 2) (make-sym 'x) (make-sym 'y))
                      (make-num 2)
                      (make-sym 'x)
                      (make-sym 'y)))
  (check-equal? (preorder (make-add (make-num 3)
                                 (make-mul (make-frac -1 2)
                                           (make-sym 'x))))
                (list (make-add (make-num 3)
                                (make-mul (make-frac -1 2)
                                          (make-sym 'x)))
                      (make-num 3)
                      (make-mul (make-frac -1 2) (make-sym 'x))
                      (make-frac -1 2)
                      (make-sym 'x)))
  )

(define-instance ((preorder add) a)
  (cons a 
        (apply append 
               (map preorder (add-addends a)))))

;; ------------
;; mul-preorder
;; ------------

(module+ test
  (check-equal? (preorder (make-mul (make-num 3) (make-sym 'x)))
                (list (make-mul (make-num 3) (make-sym 'x))
                      (make-num 3)
                      (make-sym 'x)))
  )

(define-instance ((preorder mul) m)
  (cons m 
        (apply append 
               (map preorder (mul-factors m)))))

;; --------------
;; power-preorder
;; --------------

(module+ test
  (check-equal? (preorder (make-power (make-num 3) (make-sym 'x)))
                (list (make-power (make-num 3) (make-sym 'x))
                      (make-num 3)
                      (make-sym 'x)))
  )

(define-instance ((preorder power) p)
  (cons p
        (append (preorder (power-base p))
                (preorder (power-exponent p)))))

;; ----------
;; logn-preorder
;; ----------

(module+ test
  (check-equal? (preorder (make-logn (make-num 3) (make-sym 'x)))
                (list (make-logn (make-num 3) (make-sym 'x))
                      (make-sym 'x)
                      (make-num 3)))
  )

(define-instance ((preorder logn) l)
  (cons l (append (preorder (logn-base l))
                  (preorder (logn-n l)))))

;; ------------
;; cos-preorder
;; ------------

(module+ test
  (check-equal? (preorder (make-cos (make-num 3)))
                (list (make-cos (make-num 3))
                      (make-num 3)))
  )

(define-instance ((preorder cos_) c)
  (cons c (append (preorder (cos_-n c)))))

;; ------------
;; sin-preorder
;; ------------

(module+ test
  (check-equal? (preorder (make-sin (make-num 3)))
                (list (make-sin (make-num 3))
                      (make-num 3)))
  )

(define-instance ((preorder sin_) s)
  (cons s (append (preorder (sin_-n s)))))

;; ------------
;; tan-preorder
;; ------------

(module+ test
  (check-equal? (preorder (make-tan (make-num 3)))
                (list (make-tan (make-num 3))
                      (make-num 3)))
  )

(define-instance ((preorder tan_) s)
  (cons s (append (preorder (tan_-n s)))))

;; -------------------
;; polynomial/si-preorder
;; -------------------

(module+ test
  (check-equal? (preorder (make-polynomial/si (make-sym 'x) '(0 1 2)))
                (list (make-polynomial/si (make-sym 'x) '(0 1 2))
                      (make-sym 'x)))
  )

(define-instance ((preorder polynomial/si) p)
  (list p 
        (polynomial/si-indet p)))
