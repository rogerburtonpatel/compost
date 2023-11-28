(val x 1)
(define a () x)
(val y x)
(define b () y)
