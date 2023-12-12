(datatype x ())
(define a () (let ([x 10]) x))
