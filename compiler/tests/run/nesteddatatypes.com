(datatype bit 
    ([zero ()]
     [one ()]))

(datatype bit-arr 
    ([init-arr (bit bit bit bit bit)]))

(datatype binary-tree 
    ([build-bt (bit-arr binary-tree binary-tree)]
     [nil-bt ()]))

(datatype kvpair 
    ([pair-cons (sym binary-tree)]))

(datatype kv-list 
    ([cons-kv (kvpair kv-list)]
     [nil-kv-list ()]))

;; MAIN DEFINITION

(: main (-> () unit))
(define main ()
    (let
        ([kvl (build-kv-list)])
        (print-kv-list kvl)))

;; DATATYPE LIBRARY FUNCTIONS 

(: bit-of-int (-> (int) bit))
(define bit-of-int (x)
    (if (=i (% x 2) 0)
        (zero)
        (one)))

(: int-of-bit (-> (bit) int))
(define int-of-bit (b)
    (case b
        ([(zero) 0]
         [(one) 1])))

(: init-bit-arr (-> (int) bit-arr))
(define init-bit-arr (x)
    (init-arr 
        (bit-of-int x) 
        (bit-of-int x) 
        (bit-of-int x)
        (bit-of-int x)
        (bit-of-int x)))

(: modify-bit-arr (-> (bit-arr int int) bit-arr))
(define modify-bit-arr (arr x idx)
    (let ([x-bit (bit-of-int x)])
        (case arr 
            ([(init-arr b1 b2 b3 b4 b5) 
                (if (< idx 2)
                    (if (=i idx 0)
                        (init-arr x-bit b2 b3 b4 b5)
                        (init-arr b1 x-bit b3 b4 b5))
                    (if (=i idx 2)
                        (init-arr b1 b2 x-bit b4 b5)
                        (if (=i idx 3)
                            (init-arr b1 b2 b3 x-bit b5)
                            (init-arr b1 b2 b3 b4 x-bit))))]))))

(: get-bit-arr-elem (-> (bit-arr int) int))
(define get-bit-arr-elem (arr idx)
    (case arr 
        ([(init-arr b1 b2 b3 b4 b5)
            (if (< idx 2)
                (if (=i idx 0)
                    (int-of-bit b1)
                    (int-of-bit b2))
                (if (=i idx 2)
                    (int-of-bit b3) 
                    (if (=i idx 3)
                        (int-of-bit b4) 
                        (int-of-bit b5))))])))

;; f is of a form such that it can be used as:
;; (f elem idx)
(: map-bit-arr (-> ((-> (int int) int) bit-arr) bit-arr))
(define map-bit-arr (f arr)
    (map-bit-arr-helper f arr 0))

(: map-bit-arr-helper (-> ((-> (int int) int) bit-arr int) bit-arr))
(define map-bit-arr-helper (f arr start)
    (if (>= start 5)
        arr
        (let 
            ([arr-dup (dup arr)]
             [curr-elem (get-bit-arr-elem arr-dup start)]
             [applied-elem (f curr-elem start)]
             [new-arr (modify-bit-arr arr applied-elem start)])
            (map-bit-arr-helper f new-arr (+ start 1)))))

;; APPLY HELPER FUNCTIONS 
(: is-div1 (-> (int int) int))
(define is-div1 (elem idx)
    (if (=i (% idx 1) 0)
        1
        0))

(: is-div2 (-> (int int) int))
(define is-div2 (elem idx)
    (if (=i (% idx 2) 0)
        1
        0))

(: is-div3 (-> (int int) int))
(define is-div3 (elem idx)
    (if (=i (% idx 3) 0)
        1
        0))

(: is-div4 (-> (int int) int))
(define is-div4 (elem idx)
    (if (=i (% idx 4) 0)
        1
        0))

(: is-div5 (-> (int int) int))
(define is-div5 (elem idx)
    (if (=i (% idx 5) 0)
        1
        0))

;; BUILDER FUNCTIONS FOR SUBPARTS 

(: build-bit-arr (-> ((-> (int int) int)) bit-arr))
(define build-bit-arr (f)
    (let 
        ([arr (init-bit-arr 0)])
        (map-bit-arr f arr)))

(: build-binary-tree1 (-> () binary-tree))
(define build-binary-tree1 ()
    (let 
        ([t1 (build-bt (build-bit-arr is-div1) (nil-bt) (nil-bt))] 
         [t2 (build-bt (build-bit-arr is-div2) t1 (nil-bt))]
         [t3 (build-bt (build-bit-arr is-div3) t2 (nil-bt))]
         [t4 (build-bt (build-bit-arr is-div4) t3 (nil-bt))]
         [t5 (build-bt (build-bit-arr is-div5) t4 (nil-bt))])
        t5))

(: build-binary-tree2 (-> () binary-tree))
(define build-binary-tree2 ()
    (let 
        ([t1 (build-bt (build-bit-arr is-div2) (nil-bt) (nil-bt))]
         [t3 (build-bt (build-bit-arr is-div4) (nil-bt) (nil-bt))]
         [t2 (build-bt (build-bit-arr is-div3) t1 t3)]
         [t5 (build-bt (build-bit-arr is-div1) (nil-bt) (nil-bt))]
         [t4 (build-bt (build-bit-arr is-div5) t2 t5)])
        t4))

(: build-binary-tree3 (-> () binary-tree))
(define build-binary-tree3 ()
    (let 
        ([t2 (build-bt (build-bit-arr is-div4) (nil-bt) (nil-bt))]
         [t1 (build-bt (build-bit-arr is-div3) (nil-bt) t2)]
         [t4 (build-bt (build-bit-arr is-div1) (nil-bt) (nil-bt))]
         [t5 (build-bt (build-bit-arr is-div2) t4 (nil-bt))]
         [t3 (build-bt (build-bit-arr is-div5) t1 t5)])
        t3))

(: build-binary-tree4 (-> () binary-tree))
(define build-binary-tree4 ()
    (let 
        ([t5 (build-bt (build-bit-arr is-div3) (nil-bt) (nil-bt))]
         [t3 (build-bt (build-bit-arr is-div1) (nil-bt) (nil-bt))]
         [t4 (build-bt (build-bit-arr is-div2) t3 t5)]
         [t1 (build-bt (build-bit-arr is-div4) (nil-bt) (nil-bt))]
         [t2 (build-bt (build-bit-arr is-div5) t1 t4)])
        t2))

(: build-binary-tree5 (-> () binary-tree))
(define build-binary-tree5 ()
    (let 
        ([t5 (build-bt (build-bit-arr is-div4) (nil-bt) (nil-bt))] 
         [t4 (build-bt (build-bit-arr is-div3) (nil-bt) t5)]
         [t3 (build-bt (build-bit-arr is-div2) (nil-bt) t4)]
         [t2 (build-bt (build-bit-arr is-div1) (nil-bt) t3)]
         [t1 (build-bt (build-bit-arr is-div5) (nil-bt) t2)])
        t1))

;; PRINT FUNCTIONS FOR SUBPARTS 

(: print-bit (-> (bit) unit))
(define print-bit (b)
    (case b 
        ([(zero) (print-int 0)]
         [(one) (print-int 1)])))

(: print-bit-arr (-> (bit-arr) unit))
(define print-bit-arr (arr)
    (case arr 
        ([(init-arr b1 b2 b3 b4 b5)
            (begin 
                (print-bit b1)
                (print-bit b2)
                (print-bit b3)
                (print-bit b4)
                (print-bit b5))])))

(: in-order-print (-> (binary-tree) unit))
(define in-order-print (tree)
    (case tree 
        ([(nil-bt) unit]
         [(build-bt elem left-child right-child) 
            (begin 
                (in-order-print left-child)
                (print-bit-arr elem)
                (print-newline)
                (in-order-print right-child))])))

(: print-kvpair (-> (kvpair) unit))
(define print-kvpair (pair)
    (case pair 
        ([(pair-cons k v)
            (begin 
                (print-sym k)
                (print-newline)
                (in-order-print v))])))

;; TEST SPECIFIC FUNCTIONS 

(: build-kv-list (-> () kv-list))
(define build-kv-list () 
    (cons-kv (pair-cons 'tree1' (build-binary-tree1))
        (cons-kv (pair-cons 'tree2' (build-binary-tree2))
            (cons-kv (pair-cons 'tree3' (build-binary-tree3))
                (cons-kv (pair-cons 'tree4' (build-binary-tree4))
                    (cons-kv (pair-cons 'tree5' (build-binary-tree5))
                        (nil-kv-list)))))))

(: print-kv-list (-> (kv-list) unit))
(define print-kv-list (xxs)
    (case xxs 
        ([(nil-kv-list) unit]
         [(cons-kv x xs)
            (begin 
                (print-kvpair x)
                (print-kv-list xs))])))
