;; pass in `true` to return the addition function, 
;; `false` to return the subtraction function 
(: add-or-sub (-> (bool) (-> (int int) int))) 
(define add-or-sub (cond)
    (if cond + -))

(: main (-> () unit))
(define main ()
     (begin 
         (print-int ((add-or-sub true) 2 3))
         (print-newline)
         (print-int ((add-or-sub false) 2 3))
         (print-newline)))
