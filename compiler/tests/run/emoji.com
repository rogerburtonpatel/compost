(: main (-> () unit))
(define main () 
    (let 
        ([😆 '😃'])
        (print-sym 😆)))
