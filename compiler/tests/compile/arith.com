(: main (-> () unit))
(define main ()
    (print-int (+ 1 (* 1 (/ 10 (% 6 4))))))
