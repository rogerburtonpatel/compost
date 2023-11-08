(: foo (-> () bool))
(define foo ()
    (begin
        (print-sym 'return true')
        true))

(: main (-> () unit))
(define main ()
    (if true
            (if true
                (print-sym 'true')
                (print-sym 'false'))
            (if false
                (print-sym 'false')
                (print-sym 'true'))))
    
