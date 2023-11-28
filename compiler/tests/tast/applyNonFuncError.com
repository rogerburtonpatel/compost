(val x 3)

(: main (-> () unit))
(define main () 
    (x 2))