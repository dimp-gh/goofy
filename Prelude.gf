module Prelude where

fun id x = x

fun f (x, _) = x
fun s (_, x) = x

val compose = (fn f => fn g => (fn x => (g (f x))))
val . = compose

fun not true = false
  | not false = true

fun or true = fn x => true
  | or false = fn x => x

fun and true = fn x => x
  | and false = fn x => false

fun xor true = fn x => not x
  | xor false = fn x => x