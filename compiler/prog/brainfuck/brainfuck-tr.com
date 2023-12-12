; brainfuck.com = brainfuck.org

(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

(: print-intlist (-> (intlist) unit))
(define print-intlist (listinst)
    (begin
        (print-ascii 91)
        (print-intlist-inner listinst)
        (print-newline)))

(: print-intlist-inner (-> (intlist) unit))
(define print-intlist-inner (listinst)
    (case listinst
        ([(cons-int x xs) (begin (print-int x) (print-ascii 32) (print-intlist-inner xs))]
         [(nil-intlist) (print-ascii 93)])))

(: main (-> () unit))
(define main ()
    (let
        ([inst_l (nil-intlist)]
         [inst_r (read)]
         [data_l (nil-intlist)]
         [data_r (nil-intlist)])
        (run inst_l inst_r data_l data_r 0 0)))

(: read (-> () intlist))
(define read ()
    (let
        ([i (in)])
        (if (=i i 33)
            (nil-intlist)
            (cons-int i (read)))))

(: run (-> (intlist intlist intlist intlist int int) unit))
(define run (inst_l inst_r data_l data_r mode depth)

    ;;;;;;;;;;;;;;
    ;; RUN MODE ;;
    ;;;;;;;;;;;;;;

    (if (=i mode 0)
        (case inst_r
            ([(nil-intlist)
                unit]
             [(cons-int i inst_r)

                ;;;;;;;
                ;; > ;;
                ;;;;;;;

                (if (=i i 62) ; >
                    (case data_r
                        ([(nil-intlist)
                            (run
                                (cons-int i inst_l)
                                inst_r
                                (cons-int 0 data_l)
                                (nil-intlist)
                                0 0)]
                         [(cons-int d data_r)
                             (run
                                (cons-int i inst_l)
                                inst_r
                                (cons-int d data_l)
                                data_r
                                0 0)]))

                ;;;;;;;
                ;; < ;;
                ;;;;;;;

                (if (=i i 60) ; <
                    (case data_l
                        ([(nil-intlist)
                            (run
                                (cons-int i inst_l)
                                inst_r
                                (nil-intlist)
                                (cons-int 0 data_r)
                                0 0)]
                         [(cons-int d data_l)
                             (run
                                (cons-int i inst_l)
                                inst_r
                                data_l
                                (cons-int d data_r)
                                0 0)]))

                ;;;;;;;
                ;; + ;;
                ;;;;;;;

                (if (=i i 43) ; +
                    (case data_r
                        ([(nil-intlist)
                            (run
                                (cons-int i inst_l)
                                inst_r
                                data_l
                                (cons-int 1 (nil-intlist))
                                0 0)]
                         [(cons-int d data_r)
                             (run
                                (cons-int i inst_l)
                                inst_r
                                data_l
                                (cons-int (+ d 1) data_r)
                                0 0)]))

                ;;;;;;;
                ;; - ;;
                ;;;;;;;

                (if (=i i 45) ; -
                    (case data_r
                        ([(nil-intlist)
                            (run
                                (cons-int i inst_l)
                                inst_r
                                data_l
                                (cons-int -1 (nil-intlist))
                                0 0)]
                         [(cons-int d data_r)
                             (run
                                (cons-int i inst_l)
                                inst_r
                                data_l
                                (cons-int (- d 1) data_r)
                                0 0)]))

                ;;;;;;;
                ;; . ;;
                ;;;;;;;

                (if (=i i 46) ; .
                    (case data_r
                        ([(nil-intlist)
                            (begin
                                (print-ascii 0)
                                (run
                                    (cons-int i inst_l)
                                    inst_r
                                    data_l
                                    (nil-intlist)
                                    0 0))]
                         [(cons-int d data_r)
                            (begin
                                (print-ascii d)
                                (run
                                    (cons-int i inst_l)
                                    inst_r
                                    data_l
                                    (cons-int d data_r)
                                    0 0))]))

                ;;;;;;;
                ;; , ;;
                ;;;;;;;

                (if (=i i 44) ; ,
                    (case data_r
                        ([(nil-intlist)
                            (run
                                (cons-int i inst_l)
                                inst_r
                                data_l
                                (cons-int (in) (nil-intlist))
                                0 0)]
                         [(cons-int d data_r)
                             (run
                                (cons-int i inst_l)
                                inst_r
                                data_l
                                (cons-int (in) data_r)
                                0 0)]))

                ;;;;;;;
                ;; [ ;;
                ;;;;;;;

                (if (=i i 91) ; [
                    (case data_r
                        ([(nil-intlist)
                            (run
                                (cons-int i inst_l)
                                inst_r
                                data_l
                                (nil-intlist)
                                1 0)]
                         [(cons-int d data_r)
                            (if (=i d 0)
                                (run
                                    (cons-int i inst_l)
                                    inst_r
                                    data_l
                                    (cons-int d data_r)
                                    1 0)
                                (run
                                    (cons-int i inst_l)
                                    inst_r
                                    data_l
                                    (cons-int d data_r)
                                    0 0))]))

                ;;;;;;;
                ;; ] ;;
                ;;;;;;;

                (if (=i i 93) ; ]
                    (case data_r
                        ([(nil-intlist)
                            (run
                                inst_l
                                (cons-int i inst_r)
                                data_l
                                (nil-intlist)
                                2 0)]
                         [(cons-int d data_r)
                            (if (not (=i d 0))
                                (run
                                    inst_l
                                    (cons-int i inst_r)
                                    data_l
                                    (cons-int d data_r)
                                    2 0)
                                (run
                                    (cons-int i inst_l)
                                    inst_r
                                    data_l
                                    (cons-int d data_r)
                                    0 0))]))

                ;;;;;;;;;;;;;;;;;;;;;
                ;; else, just skip ;;
                ;;;;;;;;;;;;;;;;;;;;;

                    (run
                        (cons-int i inst_l)
                        inst_r
                        data_l
                        data_r
                        0 0)))))))))]))

    ;;;;;;;;;;;;;;;;;;;;;;;
    ;; LEFT BRACKET MODE ;;    
    ;;;;;;;;;;;;;;;;;;;;;;;

    (if (=i mode 1)
        (case inst_r
            ([(nil-intlist)
                unit]
             [(cons-int i inst_r)
                (if (=i i 91)
                    (run
                        (cons-int i inst_l)
                        inst_r
                        data_l
                        data_r
                        1 (+ depth 1))
                (if (=i i 93)
                    (if (=i depth 0)
                        (run
                            (cons-int i inst_l)
                            inst_r
                            data_l
                            data_r
                            0 0)
                        (run
                            (cons-int i inst_l)
                            inst_r
                            data_l
                            data_r
                            1 (- depth 1)))
                ;; else
                    (run
                        (cons-int i inst_l)
                        inst_r
                        data_l
                        data_r
                        1 depth)))]))

    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; RIGHT BRACKET MODE ;;    
    ;;;;;;;;;;;;;;;;;;;;;;;;

        (case inst_l
            ([(nil-intlist)
                unit]
             [(cons-int i inst_l)
                (if (=i i 93)
                    (run
                        inst_l
                        (cons-int i inst_r)
                        data_l
                        data_r
                        2 (+ 1 depth))
                (if (=i i 91)
                    (if (=i depth 0)
                        (run
                            (cons-int i inst_l)
                            inst_r
                            data_l
                            data_r
                            0 0)
                        (run
                            inst_l
                            (cons-int i inst_r)
                            data_l
                            data_r
                            2 (- depth 1)))
                ;; else
                    (run
                        inst_l
                        (cons-int i inst_r)
                        data_l
                        data_r
                        2 depth)))])))))
