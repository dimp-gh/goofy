module PatternMatch where

fun fs (f, _) = f
fun sn (_, s) = s

fun main () = do {
  val p = (1, "one"),
  if (fs p) `eq` 1 then
    println "fine"
  else
    println "not fine"
}