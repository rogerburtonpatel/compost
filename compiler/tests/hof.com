(: apply (-> ((-> (int int) int) int int) int))
(define apply (f a b) (f a b))

(: main (-> () unit))
(define main ()
    (print-int (apply + 1 2)))
