(: main (-> () unit))
(define main () (print-sym 'Hello, World!'))
