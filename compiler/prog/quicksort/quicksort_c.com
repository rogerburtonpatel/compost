
(datatype list
    ([cons (int list)]
     [nil ()]))

(: filter_cmp (-> ((-> (int int) bool) int list) list))
(define filter_cmp (f n xxs)
    (case xxs
        ([(cons x xs)
            (if (f x n)
                (cons x (filter_cmp f n xs))
                (filter_cmp f n xs))]
         [(nil) (nil)])))

(: concat (-> (list list) list))
(define concat (xxs ys)
    (case xxs 
        ([(cons x xs) (cons x (concat xs ys))]
         [(nil) ys])))

(: quick_sort (-> (list) list))
(define quick_sort (l)
    (case l 
        ([(nil) (nil)]
         [(cons x xs)
            (let 
                ([lesser (filter_cmp < x (dup xs))]
                 [greater (filter_cmp >= x xs)])
                (concat 
                    (quick_sort lesser)
                    (cons x (quick_sort greater))))])))

(: print_intlist (-> (list) unit))
(define print_intlist (l)
    (case l
        ([(nil) unit]
         [(cons x xs)
            (begin 
                (print-int x)
                (print-sym ' ')
                (print_intlist xs))])))
