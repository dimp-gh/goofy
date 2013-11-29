module Math where

fun sqr x = x `times` x
fun cube x = x `times` x `times` x

fun gcd (x, 0) = x
  | gcd (x, y) = gcd (y, x `mod` y)

fun factorial 0 = 1
  | factorial x = x `times` (factorial (x `minus` 1))
