module Factorial where

fun factorial 0 = 1
  | factorial x = x `times` (factorial (x `minus` 1))

fun main () = do {
    val fact10 = factorial 10,
    do print "factorial(10) = ",
    println fact10
}