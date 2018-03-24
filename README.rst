symalg
======

This library provides functions to parse and manipulate symbolic algebraic
expressions. These expression can be constants, variables, arithmetic
operations and exponentiations. Additionally trigonometric functions and
logarithms are supported.

Expressions can be parsed from s-expressions or from infix expressions:

.. code::

  > (parse-sexpr '(expt x 2))
  (power (sum 'x) (num 2))
  > (parse-infix "x^3 + 4")
  (add (list (power (sym 'x) (num 3)) (num 4)))

The derivative of expressions can be obtained and simplified:

.. code::

  > (simplify (differentiate (parse-sexpr '(expt x 2))))
  (mul (list (num 2) (sym 'x)))
 
Expression can be converted to s-expressions, infix or LaTeX strings:

.. code::
  
  > (sexpr (parse-infix "x + 2"))
  '(+ x 2)
  > (infix (parse-sexpr '(expt x 2)))
  "(x)^(2)"
  > (latex 
      (simplify 
        (differentiate
          (parse-infix "x^4 + 2 * x^(1/2)"))))
  "\\sqrt{\\frac{1}{x}} + 4 x^{3}"

For detailed documentation see <http://docs.racket-lang.org/symalg>.

The Diophantus project using `racket-symalg` as backend can be found at
<http://johannes.tax/diophantus>.
