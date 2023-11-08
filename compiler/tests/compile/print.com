(: main (-> () unit))
(define main ()
    (begin
        (print-int -123)
        (print-sym 'hello world')
        (print-bool true)
        (print-unit unit)))
