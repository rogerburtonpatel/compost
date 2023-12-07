(: main (-> () unit))
(define main () 
    (nonexistentfun 3))