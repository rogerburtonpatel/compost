(val x y)
(val y x)
(define a () x)
(define b () y)
