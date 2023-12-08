(datatype big-data
  ([big (int big-data big-data big-data)]
  [small ()]))

(: main (-> () unit))
(define main ()
  (let ([x (big 1 (small) (small) (small))]
       [y (dup x)])
    (case x
      ([(big a b c d) unit]
      [(small) unit]))))
    
