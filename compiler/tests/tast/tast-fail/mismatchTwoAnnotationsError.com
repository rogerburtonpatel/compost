(: annotated-mismatch (-> (int) int))
(: annotated-mismatch (-> (int) sym))
(define annotated-mismatch (x) 'hi')

(: main (-> () unit))
(define main () 
    (annotated-mismatch 3))
