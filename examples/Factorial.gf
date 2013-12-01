module Factorial where

fun factorial 0 = 1
  | factorial x = x `times` (factorial (x `minus` 1))

fun main () = do {
  val _ = print "enter number: ",
  val x = readInt (),
  val f = factorial x,
  val _ = print "factorial of ",
  val _ = print x,
  val _ = print " is ",
  println f 
}