(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

(: main (-> () unit))
(define main () 
    (begin 
        (case 3 
            ([(nil-intlist) 3]))
        unit))
