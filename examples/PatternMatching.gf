module PatternMatching where

fun nameNumber 0 = "zero"
  | nameNumber 1 = "one"
  | nameNumber 2 = "two"
  | nameNumber 3 = "three"
  | nameNumber _ = "really big number"