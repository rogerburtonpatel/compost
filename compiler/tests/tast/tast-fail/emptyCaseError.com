(: main (-> () unit))
(define main () 
    (begin (case 3 ())
    unit))
    