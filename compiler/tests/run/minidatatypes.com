;; Datatype definitions 
(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

; (datatype direction 
;     ([left ()]
;      [right ()]
;      [up ()]
;      [down ()]))

; (datatype bst 
;     ([buildbst (int bst bst)]
;      [nil-bst ()]))    

; (datatype multilist 
;     ([cons-multi-int (int multilist)]
;      [cons-multi-bool (bool multilist)]
;      [nil-multilist ()]))

; (datatype twointlist 
;     ([build (intlist intlist)]))

;; General purpose functions 
(: print-endline (-> () unit))
(define print-endline () 
    (print-sym '
'))

;; Tests functionality of the `intlist` datatype 

(: test-intlist (-> () unit))
(define test-intlist ()
    (begin
        (print-sym 'Starting intlist test')
        (print-endline)
        (let 
            ([intlistinst (init-intlist)]
             [intlistinst2 (dup intlistinst)])
            (print-intlist intlistinst2)) 
        (print-sym 'Finished intlist test')
        (print-endline)))

(: init-intlist (-> () intlist))
(define init-intlist ()
    (cons-int 0 (cons-int 1 (cons-int 2 (nil-intlist)))))

(: print-intlist (-> (intlist) unit))
(define print-intlist (listinst)
    (case listinst
        ([(cons-int x xs) (begin (print-int x) (print-intlist xs))]
         [(nil-intlist) (print-endline)])))


;; Driver function 
(: main (-> () unit))
(define main ()
    (begin 
        (test-intlist)
        ; (test-direction)
        ; (test-bst)
        ; (test-multilist)
        ; (test-twointlist)
        ))



; ;; Tests functionality of the `direction` datatype 

; (: test-direction (-> () unit))
; (define test-direction ()
;     (begin 
;         (print-sym 'Starting direction test')
;         (print-endline)
;         (let 
;             ([leftdir (left)]
;              [rightdir (right)]
;              [leftdir2 (dup leftdir)])
;             (begin 
;                 (print-dir leftdir2)
;                 (print-dir rightdir)
;                 (print-endline)))
;         (print-sym 'Finished direction test')
;         (print-endline)))

; (: print-dir (-> (direction) unit))
; (define print-dir (dirinst)
;     (begin
;         (case dirinst 
;             ([(left) (print-sym 'Left')]
;              [(right) (print-sym 'Right')]
;              [(up) (print-sym 'Up')]
;              [(down) (print-sym 'Down')]))
;         (print-endline)))


; ;; Tests functionality of the bst datatype 

; (: test-bst (-> () unit))
; (define test-bst ()
;     (begin 
;         (print-sym 'Starting bst test')
;         (print-endline)
;         (let 
;             ([bst1 (init-bst)]
;              [bst2 (dup bst1)])
;             (in-order-print bst2))
;         (print-endline)
;         (print-sym 'Finishing bst test')
;         (print-endline)))

; (: init-bst (-> () bst))
; (define init-bst ()
;     (let 
;         ([leaf1 (buildbst 0 (nil-bst) (nil-bst))]  ;; Built BST is: 3
;          [leaf2 (buildbst 2 (nil-bst) (nil-bst))]  ;;              / \
;          [leaf3 (buildbst 4 (nil-bst) (nil-bst))]  ;;             1   5
;          [subtree1 (buildbst 1 leaf1 leaf2)]       ;;            / \  /
;          [subtree2 (buildbst 5 leaf3 (nil-bst))]   ;;           0  2 4
;          [root (buildbst 3 subtree1 subtree2)])
;         root))

; (: in-order-print (-> (bst) unit))
; (define in-order-print (root)
;     (case root 
;         ([(nil-bst) unit]
;          [(buildbst num left-subtree right-subtree) 
;             (begin 
;                 (in-order-print left-subtree) 
;                 (print-int num) 
;                 (in-order-print right-subtree))])))


; ;; Tests functionality of the multilist datatype 

; (: test-multilist (-> () unit))
; (define test-multilist ()
;     (begin
;         (print-sym 'Starting multilist test')
;         (print-endline)
;         (let 
;             ([multilistinst (init-multilist)]
;              [multilistinst2 (dup multilistinst)])
;             (begin (print-multilist multilistinst2)
;                 (print-multilist-ints multilistinst2)))
;         (print-sym 'Finished intlist test')
;         (print-endline)))

; (: init-multilist (-> () multilist))
; (define init-multilist ()
;     (cons-multi-int 1 
;         (cons-multi-bool true 
;             (cons-multi-int 2 
;                 (cons-multi-bool false 
;                     (nil-multilist))))))

; (: print-multilist (-> (multilist) unit))
; (define print-multilist (inst)
;     (case inst 
;         ([(cons-multi-int x xs) (begin (print-int x) (print-endline) (print-multilist xs))]
;          [(cons-multi-bool x xs) (begin (print-bool x) (print-endline) (print-multilist xs))]
;          [(nil-multilist) unit])))

; (: print-multilist-ints (-> (multilist) unit))
; (define print-multilist-ints (inst)
;     (case inst 
;         ([(cons-multi-int x xs) (begin (print-int x) (print-endline) (print-multilist-ints xs))]
;          [(cons-multi-bool _ xs) (print-multilist-ints xs)]
;          [_ unit])))

; ;; Tests functionality of a twointlist datatype 
; (: test-twointlist (-> () unit))
; (define test-twointlist ()
;     (begin
;         (print-sym 'Starting twointlist test')
;         (print-endline)
;         (let 
;             ([inst1 (init-intlist)]
;             [inst2tmp (init-intlist)]
;             [inst2 (cons-int -1 inst2tmp)]
;             [inst (build inst1 inst2)])
;             (print-twointlist inst))
;         (print-sym 'Finishing twointlist test')
;         (print-endline)))

; (: print-twointlist (-> (twointlist) unit))
; (define print-twointlist (inst)
;     (case inst 
;         ([(build l1 l2) 
;            (begin 
;                (print-sym 'List 1:')
;                (print-endline)
;                (print-intlist l1) 
;                (print-sym 'List 2:')
;                (print-endline)
;                (print-intlist l2))])))


