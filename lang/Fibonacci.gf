module Fibonacci where

fun fibo_naive 0 = 0
  | fibo_naive 1 = 1
  | fibo_naive x =
    let f1 = (fibo_naive (x `minus` 1)) in
    let f2 = (fibo_naive (x `minus` 2)) in
    f1 `plus` f2

fun main () = do {
    do print "enter n: ",
    val n = readInt (),
    do print "fibonacci of n is ",
    println (fibo_naive n)
}