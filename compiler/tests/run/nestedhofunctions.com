;; Nested functions that return functions 

(: get-get-get-print-int (-> () (-> () (-> () (-> (int) unit)))))
(define get-get-get-print-int () 
    get-get-print-int)

(: get-get-print-int (-> () (-> () (-> (int) unit))))
(define get-get-print-int ()
    get-print-int)

(: get-print-int (-> () (-> (int) unit)))
(define get-print-int () 
    print-int)

;; Nested functions that apply functions 

(: apply-arith (-> ((-> (int int) int) int int) int))
(define apply-arith (f x y)
    (f x y))

(: apply-apply-arith (-> ((-> ((-> (int int) int) int int) int) (-> (int int) int) int int) int))
(define apply-apply-arith (f g x y)
    (f g x y))

(: apply-apply-apply-arith (-> ((-> ((-> ((-> (int int) int) int int) int) (-> (int int) int) int int) int) 
                                (-> ((-> (int int) int) int int) int) 
                                (-> (int int) int) 
                                int 
                                int) 
                            int))
(define apply-apply-apply-arith (f g h x y)
    (f g h x y))

;; Combines both types of ho functions 
(: get-greater (-> ((-> (int int) int) (-> (int int) int) int int) (-> (int int) int)))
(define get-greater (f g x y)
    (if (> (f x y) (g x y))
        f 
        g))

;; Main

(: main (-> () unit))
(define main ()
    (begin 
        ((((get-get-get-print-int))) 2)
        (print-int (apply-apply-apply-arith apply-apply-arith apply-arith + 2 3))
        (print-int ((get-greater + * 2 3) 2 3))
        (print-int ((get-greater + * 1 2) 1 2))
        (print-int ((get-greater + * 2 2) 2 2))))
