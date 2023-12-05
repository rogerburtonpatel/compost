(datatype int-list 
    ([cons-int (int int-list)]
     [nil-int-list ()]))

(val x (cons-int 3 (cons-int 2 (cons-int 1 (nil-int-list)))))
(val q (cons-int 9 (cons-int 8 (cons-int 7 (nil-int-list)))))

(: sum (-> (int-list) int))
(define sum (l)
    (case l
        ([(cons-int i is) (+ i (sum is))]
         [(nil-int-list) 0])))

(: a (-> (int-list) int-list))
(define a (x)
    (let ([y (cons-int 4 x)]
          [z (sum y)])
         (cons-int z x)))

(: main (-> () unit))
(define main ()
    (print-int (sum (a q))))


(: foo (-> () int))
(define foo () 
  (let ([x (cons-int 1 (nil-int-list))]
        [x (cons-int 2 (nil-int-list))]
        [y (sum x)]
        [z (+ y (sum x))])
       z))
