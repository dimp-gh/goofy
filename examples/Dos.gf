module Dos where


fun foo (0, 0) = do {
    do println "foo() got (",
    do println 0,
    do println ", ",
    do println 0,
    println ")"
} | foo (1, x) = do {
    do println "foo got (1, ",
    do println x,
    println ")" 
} | foo (x, y) = (println (x `plus` y))

fun bar x = do {
    do print "x is ",
    println x
}

fun baz (x, y) = do {
    do print "arg is a tuple (",
    do print x,
    do print ", ",
    do print y,
    println ")"
}

fun baq (1, x) = do {
    do print "arg is a tuple (1, ",
    do print x,
    println ")"
}

fun main () = do {
    do foo (0, 0),
    do foo (1, 2),
    foo (2, 3)
}