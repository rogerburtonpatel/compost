(datatype intlist ([cons-int (int intlist)] [nil-intlist ()]))
(datatype symlist ([cons-sym (sym symlist)] [nil-symlist ()]))
(: main (-> () unit))
(define main () 
    (let ([hi 
        (case (cons-int 3 (nil-intlist)) 
            (
            [(cons-int x xs) x] 
            [(nil-intlist) 3]
            [(nil-intlist) 3]
            ))]
            [hi2 
        (case (cons-int 3 (nil-intlist)) 
            (
            [(cons-int x xs) x] 
            ))]
            [hi3 
        (case (cons-int 3 (nil-intlist)) 
            (
            [_ 3] 
            [_ 4] 
            ))]
            ) 
    unit))
