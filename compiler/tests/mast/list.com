;; Datatype definitions 
(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

;; General purpose functions 
(: print-endline (-> () unit))
(define print-endline () 
    (print-sym '
'))

;; Driver function 
(: main (-> () unit))
(define main ()
    (let ([listinst (cons-int 1 (nil-intlist))]
          [listinst2 (dup listinst)])
         unit))
