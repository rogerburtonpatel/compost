(: main (-> () unit))
(define main ()
    (begin
        (print-sym 'gcd of 100 and 101 is: ')))

(: gcd (-> (int int) int))
(define gcd (a b)
    (if (i= a 0)
        b
        (gcd (% b a) a)))
