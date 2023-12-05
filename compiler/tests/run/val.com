(val x 3)
(val y (print-sym '
'))

(: main (-> () unit))
(define main () 
    (begin (print-int x)
        y))

