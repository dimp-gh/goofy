module FunDefs where

val foo = fn x => + x

fun bar x = + x

fun main () = if (foo 3 5) `eq` (bar 3 5) then () else (error "bad bad 
bad")
