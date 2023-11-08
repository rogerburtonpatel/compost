(: count-down (-> (int) unit))
(define count-down (x)
    (begin
        (print-int x)
        (count-down (- x 1))))

(: main (-> () unit))
(define main () (count-down (in)))
