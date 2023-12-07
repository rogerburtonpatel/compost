(: print-sym (-> (sym) unit))

(: main (-> () unit))
(define main () 
    (print-sym s))