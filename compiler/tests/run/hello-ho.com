;; "higher order" hello world
;; This is very much a flex

(: hello (-> (sym) unit))
(define hello (message)
    (begin
        (print-sym 'Hello, ')
        (print-sym message)))

(: apply1 (-> ((-> (sym) unit) sym) unit))
(define apply1 (f s) (f s))

(: main (-> () unit))
(define main () 
    (apply1 hello 'World!'))
