(: annotated-mismatch (-> (int) int))
(define annotated-mismatch (x) 'hi')

(: main (-> () unit))
(define main () 
    (annotated-mismatch 3))
