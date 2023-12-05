(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

(val intlist 10)

(: main (-> () unit))
(define main () (test intlist))

(: test (-> (int) unit))
(define test (main)
    (let (
             [bar 3]
             [intlist (nil-intlist)]
         )
         (case intlist (
            [(cons-int nt intlist) (print-int nt)]
            [(nil-intlist) (print-int (+ main bar))]
         ))
    )
)
