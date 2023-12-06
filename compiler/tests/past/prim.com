(val + test)

(: main (-> (int) unit))
(define main () (print-int (+ 2 1)))

(: test (-> (int int) int))
(define test (a b) (- a b))
