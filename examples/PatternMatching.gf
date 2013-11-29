module PatternMatching where

fun nameNumber 0 = "zero"
  | nameNumber 1 = "one"
  | nameNumber 2 = "two"
  | nameNumber 3 = "three"
  | nameNumber _ = "really big number"

fun numberName "zero" = 0
  | numberName "one" = 1
  | numberName "two" = 2
  | numberName _ = error "i do not know that number"
