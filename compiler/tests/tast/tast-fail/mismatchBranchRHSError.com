(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

(: main (-> () unit))
(define main () 
    (begin 
        (case (cons-int 3 (nil-intlist)) 
              ([(cons-int x xs) x]
               [(nil-intlist) 'i am so of the wrong type HA!']))
        unit))