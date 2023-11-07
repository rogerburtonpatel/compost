(: f (-> () unit))
(define f ()
    (begin
        (print-sym 'f called')
        (g)))

(: g (-> () unit))
(define g ()
    (begin
        (print-sym 'g called')
        (f)))

(: main (-> () unit))
(define main () (f))
