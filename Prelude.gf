module Prelude where

fun id x = x

fun f (x, _) = x
fun s (_, x) = x

val compose = (fn f => fn g => (fn x => (g (f x))))
val . = compose