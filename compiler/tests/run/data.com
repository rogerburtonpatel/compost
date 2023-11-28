(datatype int-list
  ([int-cons (int int-list)]
  [int-nil ()]))

(: print-int-list (-> (int-list) unit))
(define print-int-list (xxs)
    (case xxs
      ([(int-cons x xs)
         (begin
            (print-sym '(int-cons ')
            (print-int x)
            (print-sym ' ')
            (print-int-list xs)
            (print-sym ')'))]
       [(int-nil) (print-sym 'int-nil')])))

(: main (-> () unit))
(define main () (print-int-list (int-cons 1 (int-nil))))
