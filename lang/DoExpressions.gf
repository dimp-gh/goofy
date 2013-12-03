module DoExpressions where

# do-expression consists of a bunch of statements (separated with commas)
# and a resulting expression.
fun foo () = do {
  do println "simple do example:",
  do print "  ",
  fun bar x = + x, # you can define local functions inside do-expressions
  println (bar 5 10)
}

# nested do-expressions are fine (expect they look cumbersome)
fun nestedDos () = do {
  do println "nested do-expressions are fine:",
  do println "  outside1",
  do (do {
    do println "  inside1",
    println "  inside2"
  }),
  println "  outside2"    
}

fun main () = do {
  do foo (),
  nestedDos ()
}