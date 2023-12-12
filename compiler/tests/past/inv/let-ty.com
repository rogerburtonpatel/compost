(define a () (let ([x 10]) x))
(datatype x ())
