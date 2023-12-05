(: max (-> (int int) int))
(define max (x y)
  (if (> x y)
      x
      y
  )
)

(: main (-> () unit))
(define main ()
    (begin 
        (print-int (max -1 1))
        (print-int (max 1 -1))
        (print-int (max 1 1))))
