letrec fact = (fn n => if (zero n) then 1 else (n `times` (fact (pred n)))) in (println (fact 5))
