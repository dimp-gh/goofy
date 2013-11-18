(letrec fact = (fn n => (((if (zero n)) 1) ((times n) (fact (pred n))))) in (println (fact 5)))
