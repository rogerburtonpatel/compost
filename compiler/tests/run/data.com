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
       [(int-nil) (print-sym '(int-nil)')])))

(: main (-> () unit))
(define main ()
  (let ([x (int-cons 1 (int-cons 2 (int-nil)))]
        [y (dup x)])
    (begin
        (print-int-list x)
        (print-newline)
        (print-int-list y))))
