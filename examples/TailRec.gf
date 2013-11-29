module TailRec where

fun factorial' (0, acc) = acc
  | factorial' (x, acc) = factorial' (x `minus` 1, acc `times` x)

fun factorial x = factorial' (x, 1)

fun main _ = println (factorial 12)