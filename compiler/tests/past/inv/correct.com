(val a 10)
(datatype b 
    ([c (d b)]
     [e ()]))
(val f 20)
(define g (f)
    (begin
        (let
           ([a 100]
            [c 200]
            [e 300]
            [f 400])
           (a))
        (a)
        (case f
            ([(c f a)
                 a]
             [_
                 1]))
        (f)))
