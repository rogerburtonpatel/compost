(datatype intlist 
    ([cons (int intlist)]
     [nil ()]))

(: car (-> (intlist) int))
(define car (xxs)
  (case xxs
        (
          [(cons x xs) x]
          [(nil) (begin (error 'tried to take car of nil') 1)]
        )
  )
)
(: main (-> () unit))
(define main ()
  (let
    (
      [xs (cons 1 (nil))]
      [y (car xs)]
    )
    (print-int y)
  )
)