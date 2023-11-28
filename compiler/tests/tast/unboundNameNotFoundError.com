(: main (-> () unit))
(define main () 
    (print-sym s))