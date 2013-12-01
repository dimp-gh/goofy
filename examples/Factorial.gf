# every module must start with the following header
module Factorial where

# standard recursive definition of factorial
fun factorial 0 = 1
  | factorial x = x `times` (factorial (x `minus` 1))

# Example of do-expression.

# Do-expression consists of a list of statements (separated with commas) and ends with an expression.

# When it is evaluated, all statements are evaluated first, and then the last expression is evaluated. Result of evaluation of the last expression is the result of evaluation of do-expression. 

# You can also evaluate expressions inside of do-expression by using binding to underscore variable.
# Examples: `val _ = (println "hello, world")`.

# This construct is so common that Goofy even provides syntactic sugar for it.
# That is, notation `do <expression>` desugars exactly to `val _ = <expression>`.
# This syntactic sugar works only inside of do-expressions.

fun main () = do {
  do print "enter number: ", # take a look, statements in do-expression must be separated with commas
  val x = readInt (),
  val f = factorial x,
  do print "factorial of ",
  do print x,
  do print " is ",
  println f # every do-expression must end with expression, which is a result value of a do-expression
}