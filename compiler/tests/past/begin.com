(define x () (begin (dup one) (dup two) (dup three) four five))
