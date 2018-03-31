#lang scribble/manual

@(require racket/require
          (for-label symalg 
                     (subtract-in
                       (multi-in racket/base)
                       symalg))
          (for-label racket)
          scribble/eval)

@require[@for-label[symalg
                    racket/base]]

@title{Symbolic algebraic expressions}
@author{Johannes Tax}

@defmodule[symalg]

This library provides functions to parse and manipulate symbolic algebraic
expressions. These expression can be constants, variables, arithmetic
operations and exponentiations. Additionally trigonometric functions and
logarithms are supported.

@section{Example}

@(interaction
  #:eval ((make-eval-factory '(symalg)))
  #:escape unsyntax

  (define expr (parse-infix "3*x^2 - 4*x + cos(x)"))
  (define expr-deriv (simplify 
                       (differentiate expr)))
  expr-deriv
  (infix expr-deriv)
  (sexpr expr-deriv)
  (latex expr-deriv)
  (define f (evaluate expr-deriv))
  (f 3)
)

@section{API Reference}

@defproc[(parse-sexpr [s any/c]) 
          symalg-expr?]{
Parses an s-expression and returns a corresponding symbolic algebraic 
expression. @racket[s] can be an expression @racket[expr] of the following 
form:

@centered{
@tabular[#:sep @hspace[1]
         (list (list @verbatim{expr :} @verbatim{number?})
               (list ""   @verbatim{| symbol?})
               (list ""   @verbatim{| e})
               (list ""   @verbatim{| pi})
               (list ""   @verbatim{| (+ expr ...+)})
               (list ""   @verbatim{| (- expr ...+)})
               (list ""   @verbatim{| (* expr ...+)})
               (list ""   @verbatim{| (/ expr expr)})
               (list ""   @verbatim{| (expt expr expr)})
               (list ""   @verbatim{| (log expr expr)})
               (list ""   @verbatim{| (sin expr)})
               (list ""   @verbatim{| (cos expr)})
               (list ""   @verbatim{| (tan expr)})
        )]
}
}

@defproc[(parse-infix [s string?]) 
          symalg-expr?]{
Parses a string containing an infix expression and returns a corresponding 
symbolic algebraic expression. An infix expression can be an expression 
@racket[expr] of the following form:

@centered{
@tabular[#:sep @hspace[1]
         (list (list @verbatim{expr :} @verbatim{number?})
               (list ""   @verbatim{| symbol?})
               (list ""   @verbatim{| e})
               (list ""   @verbatim{| pi})
               (list ""   @verbatim{| (expr)})
               (list ""   @verbatim{| expr + expr})
               (list ""   @verbatim{| expr - expr})
               (list ""   @verbatim{| expr * expr})
               (list ""   @verbatim{| expr / expr})
               (list ""   @verbatim{| expr ^ expr})
               (list ""   @verbatim{| log(expr, expr)})
               (list ""   @verbatim{| sin(expr)})
               (list ""   @verbatim{| cos(expr)})
               (list ""   @verbatim{| tan(expr)})
        )]
}

}

@defproc[(symalg-expr? [e any/c]) 
          boolean?]{
This predicate checks, if the argument denotes a symbolic algebraic expression 
that can be processed by the functions below.
}

@defproc[(simplify [e symalg-expr?])
         symalg-expr?]{
Returns a simplified form of @racket[e]. Simplification is mostly based on 
Joel S. Cohen's @italic{Computer Algebra and Symbolic Computation}.

Some examples:

@(interaction
  #:eval ((make-eval-factory '(symalg)))
  #:escape unsyntax
(infix (simplify (parse-infix "x+x")))
(infix (simplify (parse-infix "x^0")))
(infix (simplify (parse-infix "2*x^2 + 4*x^2 + 5 - 6")))
(infix (simplify (parse-infix "2*x^2 / x")))
(infix (simplify (parse-infix "2^x^4")))
)
}


@defproc[(differentiate [e symalg-expr?])
         symalg-expr?]{
The function @racket[differentiate] computes the first derivation of a given 
symbolic algebraic expression.

Take into account that the resulting expression is not simplified 
automatically, a further call to @racket[simplify] is necessary to bring it into 
a canonical form:

@(interaction
  #:eval ((make-eval-factory '(symalg)))
  #:escape unsyntax

(define expr (parse-infix "2*x^2 - x")) 
(infix (differentiate expr))
(infix (simplify (differentiate expr))))
}


@defproc[(evaluate [e symalg-expr?])
         (f number? ...+)]{
Returns a function that evaluates the given symbolic algebraic expression. 
Parameters of the returned function are bound to variables (unbound symbols) of 
the symbolic algebraic expression in alphabetical order.
}


@defproc[(infix [e symalg-expr?])
        string?]{
Returns the infix representation of a symbolic algebraic expression.
}

@defproc[(latex [e symalg-expr?])
         string?]{
Returns the LaTeX math mode representation of a symbolic algebraic expression.
}

@defproc[(sexpr [e symalg-expr?])
         any/c]{
Returns the s-expression representation of a symbolic algebraic expression.
}
