(: print-a-symbol (-> (sym) unit))

(define print-a-symbol (s)
    (print-sym s))

(: main (-> () unit))
(define main () 
    (print-a-symbol 3))