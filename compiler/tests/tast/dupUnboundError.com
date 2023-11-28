(: main (-> () unit))
(define main () 
    (begin (dup x)
    unit))