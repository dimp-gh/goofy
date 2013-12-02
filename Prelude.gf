module Prelude where

# Generic identity function, just because
fun id x = x

# Usage of pattern-matching to unpack pairs 
fun f (x, _) = x
fun s (_, x) = x

# Function composition
val compose = (fn f => fn g => (fn x => (g (f x))))
val . = compose

# Standard logic functions
fun not true = false
  | not false = true

fun or true = fn x => true
  | or false = fn x => x

fun and true = fn x => x
  | and false = fn x => false

fun xor true = fn x => not x
  | xor false = fn x => x

# Implementation of strings to numbers conversion
fun charToInt' "0" = 0
  | charToInt' "1" = 1
  | charToInt' "2" = 2
  | charToInt' "3" = 3
  | charToInt' "4" = 4
  | charToInt' "5" = 5
  | charToInt' "6" = 6
  | charToInt' "7" = 7
  | charToInt' "8" = 8
  | charToInt' "9" = 9
  | charToInt' _ = error "charToInt' got non-digit character"
# take a look, we used halting function `error` right above

fun charToInt x =
  if (length x) `eq` 1 then
    charToInt' x
  else
    error "charToInt can only handle 1-character strings"

# example of tail recursion
fun strToInt' (x, acc)  =
  if (length x) `eq` 0 then
    acc
  else
    strToInt' (tail x, (acc `times` 10) `plus` (charToInt (head x)))

fun strToInt x = strToInt' (x, 0)

val readInt = read `compose` strToInt
# equivalent to: fun readInt () = strToInt (read ())

# Function `read` has the following signature:
# read :: Unit -> String
# So, when you call it, you should not omit unit literal.

# This function is here for chaining Unit-typed functions.
# Example of usage:
# (print N) `andThen` (print " bottles of beer ") `andThen` (println "on the wall")
# Not as powerful as do-expressions, but hey, we can write it in Goofy.
fun andThen _ = fn _ => ()