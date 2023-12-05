(datatype bst 
    ([buildbst (int bst bst)]
     [nil-bst ()]))

(: test-bst (-> () unit))
(define test-bst ()
    (begin 
        (print-sym 'Starting bst test')
        (print-newline)
        (let 
            ([bst1 (init-bst)]
             [bst2 (dup bst1)])
            (in-order-print bst2))
        (print-newline)
        (print-sym 'Finishing bst test')
        (print-newline)))

(: init-bst (-> () bst))
(define init-bst ()
    (let 
        ([leaf1 (buildbst 0 (nil-bst) (nil-bst))]  ;; Built BST is: 3
         [leaf2 (buildbst 2 (nil-bst) (nil-bst))]  ;;              / \
         [leaf3 (buildbst 4 (nil-bst) (nil-bst))]  ;;             1   5
         [subtree1 (buildbst 1 leaf1 leaf2)]       ;;            / \  /
;;         [subtree2 (buildbst 5 leaf3 (nil-bst))]   ;;           0  2 4
;;         [root (buildbst 3 subtree1 subtree2)])
;;        root))
) subtree1))

(: in-order-print (-> (bst) unit))
(define in-order-print (root)
    (case root 
        ([(nil-bst) unit]
         [(buildbst num left-subtree right-subtree) 
            (begin 
                (in-order-print left-subtree) 
                (print-int num) 
                (in-order-print right-subtree))])))

;; Driver function 
(: main (-> () unit))
(define main () (test-bst))

