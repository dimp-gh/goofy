# every module must start with the following header
module Factorial where

# standard recursive definition of factorial
fun factorial 0 = 1
  | factorial x = x `times` (factorial (x `minus` 1))

# example of do-expression
# these `val _ = ...` constructs are not very pretty, but current Goofy
# grammar allows only statements inside of do-expression
fun main () = do {
  val _ = print "enter number: ", # take a look, statements in do-expression must be separated with commas
  val x = readInt (),
  val f = factorial x,
  val _ = print "factorial of ",
  val _ = print x,
  val _ = print " is ",
  println f # every do-expression must end with expression, which is a result value of a do-expression
}