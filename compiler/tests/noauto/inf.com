(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

(: main (-> () unit))
(define main ()
    (foo (cons-int 1 (cons-int 2 (nil-intlist)))))

(: foo (-> (intlist) unit))
(define foo (xs)
    (case xs
        ([(nil-intlist) unit]
         [(cons-int x xs)
            (let
                ([x
                    (if (> x 1000)
                        0
                        (+ x 1))])
                (foo (cons-int x xs)))])))
