(set-logic LIA)
(synth-fun f ((x Int) (y Int)) Int
  ((I Int) (Ic List)) 
  ((I Int (0 1 x y
            (+ I I)
            (* Ic I)))
   (Ic Int (0 1 2 (- 1) (- 2)))))
(declare-var x Int)
(declare-var y Int)
(constraint (= (f x y) (* 2 (+ x y))))
(check-synth)