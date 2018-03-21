#lang racket/base

;; Operations on linear algebraic expressions.

(provide linear?)

;; ---------------------------------
;; Import and implementation section

(require multimethod
         racket/set
         "private/preorder.rkt"
         "private/data.rkt")

;; -------
;; linear?
;; -------

(module+ test
  (require rackunit
           "parse.rkt")

  (check-true  (linear? (parse-sexpr '(+ x 9))))
  (check-true  (linear? (parse-sexpr '(+ y 9))))
  (check-false (linear? (parse-sexpr '(+ x y))))
  )

(define (linear? e)
  (define syms (filter sym? (preorder e)))
  (>= 1 (length (set->list (apply set syms)))))
