(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))


(: usescon (-> (int) unit))
(define usescon (cons-int) unit)

(: main (-> () unit))
(define main () 
    (let ([cons-int 1])
    unit))
    