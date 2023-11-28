(: takes-sym-and-int (-> (sym int) unit))

(define takes-sym-and-int (s i)
    (print-sym s))

(: main (-> () unit))
(define main () 
    (takes-sym-and-int 'hi'))