module RunMe where

fun triangle 1 = 1
  | triangle x = x `plus` (triangle (x `minus` 1))

fun main _ = println (triangle 13)