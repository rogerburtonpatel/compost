(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

(datatype symlist 
    ([cons-sym (sym symlist)]
     [nil-symlist ()]))

(: main (-> () unit))
(define main () 
    (begin 
        (case (cons-int 3 (nil-intlist)) 
              ([(cons-int x xs) x]
               [x 3]
            ;    [(nil-intlist) 3]
               ))
        unit))