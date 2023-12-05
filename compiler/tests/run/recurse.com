(: count-down (-> (int) unit))
(define count-down (x)
    (if (=i x 0)
        unit 
        (begin
            (print-int x)
            (print-sym '
'               ) ;; prints newline character
            (count-down (- x 1)))))

(: main (-> () unit))
(define main () (count-down 10)) 
