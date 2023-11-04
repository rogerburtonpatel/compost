(: foo (-> () bool))
(define foo ()
    (begin
        (print-sym 'return true')
        true))

(: main (-> () unit))
(define main ()
    (if (i= 1 1) 
            (if true (print-sym 'true') (print-sym 'false'))
            (print-sym '2')))
    
