module Fibonacci where

# naive fibonacci implementations
# can't even calculate fibonacci(25)
fun fibo_naive 0 = 0
  | fibo_naive 1 = 1
  | fibo_naive x =
    let f1 = (fibo_naive (x `minus` 1)) in
    let f2 = (fibo_naive (x `minus` 2)) in
    f1 `plus` f2

fun fibo_tail x = do {
  fun helper (0, fibs) = (fst fibs)
    | helper (x, fibs) =
      let x1 = x `minus` 1 in
      let f1 = (snd fibs) in
      let f2 = (fst fibs) `plus` f1 in
      helper (x1, (f1, f2)),
   helper (x, (0, 1))
}

fun main () = do {
    do println (fibo_naive 11),
    println (fibo_tail 25)
}