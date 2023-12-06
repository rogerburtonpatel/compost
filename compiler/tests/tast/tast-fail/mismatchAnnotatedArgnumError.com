(: annotated-mismatch (-> (int) int))
(define annotated-mismatch (x y) 'hi')

(: main (-> () unit))
(define main () 
    (annotated-mismatch 3))
