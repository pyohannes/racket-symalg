#lang info
(define collection "symalg")
(define deps '("base" 
               "multimethod"
               "parser-tools"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/symalg.scrbl" ())))
(define pkg-desc "Manipulate symbolic algebraic expressions")
(define version "0.1")
(define pkg-authors '(pyohannes))
