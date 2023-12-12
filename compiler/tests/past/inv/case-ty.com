(define a (b)
    (case b
        ([(cons x xs)
             2]
         [_
             1])))
(datatype x ())
