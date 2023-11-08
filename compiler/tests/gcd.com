(: main (-> () unit))
(define main ()
    (let 
        ([x (in)]
         [y (in)])
        (begin
            (print-sym 'gcd of ')
            (print-int x)
            (print-sym ' and ')
            (print-int y)
            (print-sym ' is: ')
            ;(print-int (gcd x y)))
            
            )))

(: gcd (-> (int int) int))
(define gcd (a b)
    (if (i= a 0)
        b
        (gcd (% b a) a)))
