;; Defines intlist datatype
(datatype intlist 
    ([cons-int (int intlist)] ;; nonempty list constructor
     [nil-intlist ()])) ;; empty list constructor

;; Function: get length of inputted intlist
(: len-intlist (-> (intlist) int))
(define len-intlist (xxs)
    (case xxs
        ([(cons-int x xs) (+ 1 (len-intlist xs))]
         [(nil-intlist) 0])))

(val len3list (cons-int 0 (cons-int 1 (cons-int 2 (nil-intlist)))))

(: main (-> () unit)) ;; Type annotation
(define main () ;; Entry point
    (print-int (len-intlist len3list)))
