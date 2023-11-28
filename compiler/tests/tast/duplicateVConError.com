(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

(datatype intlist2 
    ([cons-int2 (int intlist2)]
     [nil-intlist ()])) ; oops! 

(: main (-> () unit))
(define main () 
    (begin 
        (case (cons-int 3 (nil-intlist)) 
              ([(cons-int x xs) x]
               [(nil-symlist) 3]))
        unit))