;; Datatype definitions 
(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

;; Driver function 
(: main (-> () unit))
(define main ()
    (let ([listinst (cons-int 1 (nil-intlist))]
          [listinst2 (dup listinst)])
         (case listinst
            ([(cons-int x xs) unit]
             [_ unit]))))
