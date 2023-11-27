(datatype bool2
  ([true2 ()]
  [false2 ()]))

(: main (-> () unit))
(define main () unit)
