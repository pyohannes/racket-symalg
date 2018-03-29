#lang racket/base

;; Predicate that checks for symbolic algebraic expressions

(provide symalg-expr?)

;; ---------------------------------
;; Import and implementation section

(require "private/data.rkt")

;; ------------
;; symalg-expr?
;; ------------

(module+ test
  (require rackunit
           "parse.rkt")

  (check-true  (symalg-expr? (make-num 1)))
  (check-true  (symalg-expr? (make-frac 1 2)))
  (check-true  (symalg-expr? (make-sym 'x)))
  (check-true  (symalg-expr? (make-constant 'pi)))
  (check-true  (symalg-expr? (parse-sexpr '(+ x 1))))
  (check-true  (symalg-expr? (parse-sexpr '(- x 2))))
  (check-true  (symalg-expr? (parse-sexpr '(* x 2))))
  (check-true  (symalg-expr? (parse-sexpr '(expt x 2))))
  (check-true  (symalg-expr? (parse-sexpr '(logn x 3))))
  (check-true  (symalg-expr? (parse-sexpr '(cos x))))
  (check-true  (symalg-expr? (parse-sexpr '(sin x))))
  (check-true  (symalg-expr? (parse-sexpr '(tan x))))
  (check-false (symalg-expr? 1))
  (check-false (symalg-expr? 'x))
  (check-false (symalg-expr? (list (make-num 1))))
  )

(define (symalg-expr? e)
  (ormap (lambda (f)
           (f e))
         (list num? frac? sym? constant? add? mul? power? logn? cos_? sin_? 
               tan_?)))
