(val x 'error message here')
(: car (-> (intlist) int))
(define car (xxs)
  (case xxs
        (
          [(cons x xs) x]
          [(nil) (error x)]
        )
  )
)
