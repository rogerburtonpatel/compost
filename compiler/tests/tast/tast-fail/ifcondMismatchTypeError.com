(: main (-> () unit))
(define main () 
    (begin (let ([notabool 1])
    (if notabool 3 4))
    unit))
    