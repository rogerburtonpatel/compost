(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

(: main (-> () unit))
(define main () 
    (begin 
        (case (cons-int 3 (nil-intlist)) 
            ([(nonexistentcon) 3]))
        unit))
    