
(datatype int-list 
    ([cons-int (int int-list)]
     [nil-int-list ()]))

(: main (-> () unit))
(define main ()
    (let 
        ([l (cons-int 4 (cons-int 2 (cons-int 5 (cons-int 3 (cons-int 1 (nil-int-list))))))]
         [l-sorted (quick-sort l)])
        (print-intlist l-sorted)))

(: filter-ge (-> (int int-list) int-list))
(define filter-ge (n xxs)
    (case xxs
        ([(cons-int x xs) 
            (if (>= x n) 
                (cons-int x (filter-ge n xs))
                (filter-ge n xs))]
         [(nil-int-list) (nil-int-list)])))

(: filter-lt (-> (int int-list) int-list))
(define filter-lt (n xxs)
    (case xxs
        ([(cons-int x xs) 
            (if (< x n) 
                (cons-int x (filter-lt n xs))
                (filter-lt n xs))]
         [(nil-int-list) (nil-int-list)])))

(: concat (-> (int-list int-list) int-list))
(define concat (xxs ys)
    (case xxs 
        ([(cons-int x xs) (cons-int x (concat xs ys))]
         [(nil-int-list) ys])))

(: quick-sort (-> (int-list) int-list))
(define quick-sort (l)
    (case l 
        ([(nil-int-list) (nil-int-list)]
         [(cons-int x xs) 
            (let 
                ([lesser (filter-lt x (dup xs))]
                 [greater (filter-ge x xs)])
                (concat 
                    (quick-sort lesser)
                    (cons-int x (quick-sort greater))))])))

(: print-intlist (-> (int-list) unit))
(define print-intlist (l)
    (case l
        ([(nil-int-list) unit]
         [(cons-int x xs) 
            (begin 
                (print-int x)
                (print-intlist xs))])))
