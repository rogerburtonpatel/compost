(define unannotated (x) (+ x 1))

(: main (-> () unit))
(define main () 
    (unannotated 3))
