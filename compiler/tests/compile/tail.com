(: slow-mul-tail (-> (int int int) int))
(define slow-mul-tail (n i acc)
    (if (i= i 0)
        acc
        (slow-mul-tail n (- i 1) (+ acc n))))

(: slow-mul (-> (int int) int))
(define slow-mul (x y)
    (slow-mul-tail x y 0))

(: main (-> () unit))
(define main () (print-int (slow-mul 1 2147483647))) 
