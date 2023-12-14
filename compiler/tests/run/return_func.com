(: get-print-int (-> () (-> (int) unit)))
(define get-print-int ()
    print-int)

(: main (-> () unit))
(define main ()
    ((get-print-int) 2))
