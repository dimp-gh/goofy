letrec gcd = (fn m => (fn n => if zero (m `mod` n) then n else (gcd n 
(m `mod` n)))) in (println (gcd 15 95))
