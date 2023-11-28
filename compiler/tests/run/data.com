(datatype int-list
  ([int-list (int int-list)]
  [int-nil ()]))

(: main (-> () unit))
(define main () (print-sym 'foobar'))
;;    (case (true2)
;;        ([(true2) (print-sym 'true2')]
;;         [(false2) (print-sym 'false2')])))
