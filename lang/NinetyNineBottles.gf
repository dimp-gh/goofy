module NinetyNineBottles where

fun bottlesToStr 0 = "no more bottles"
  | bottlesToStr 1 = "1 bottle"
  | bottlesToStr x = (intToStr x) `append` " bottles" 

fun bottles' (0, orig) = do {
  do println "No more bottles of beer on the wall, no more bottles of beer.",
  do print "Go to the store and buy some more, ",
  println ((bottlesToStr orig) `append` " of beer on the wall.")
} | bottles' (x, orig) = do {
  val num = bottlesToStr x,
  val next = bottlesToStr (x `minus` 1),
  do print (num `append` " of beer on the wall, "),
  do println (num `append` " of beer."),
  do print "Take one down and pass it around, ",
  do println (next `append` " of beer on the wall."),
  do println "",
  bottles' (x `minus` 1, orig)
}

fun bottles x = bottles' (x, x)

fun main () = bottles 99