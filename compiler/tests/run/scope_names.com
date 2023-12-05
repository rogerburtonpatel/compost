(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

(val ilval 10)

(: main (-> () unit))
(define main () (test ilval))

(: test (-> (int) unit))
(define test (m)
    (let (
             [bar 3]
             [illet (nil-intlist)]
         )
         (case illet (
            [(cons-int nt is) (print-int nt)]
            [(nil-intlist) (print-int (+ m bar))]
         ))
    )
)
