(: fib (-> (int) int))
(define fib (n)
    (if (i= n 0)
        0
        (if (i= n 1)
            1
            (+ (fib (- n 1)) (fib (- n 2))))))

(: main (-> () unit))
(define main ()
    (print-int (fib (in))))
