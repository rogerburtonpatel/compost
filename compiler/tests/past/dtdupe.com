(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

(val cons-int 10)

(: main (-> () unit))
(define main () (print-int cons-int))
