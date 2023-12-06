(: main (-> () unit))
(define main () 
    (if true
    3
    'i am diabolically the wrong type >:]'))