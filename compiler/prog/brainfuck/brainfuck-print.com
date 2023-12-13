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

(: size-intlist (-> (intlist) int))
(define size-intlist (listinst)
    (case listinst
        ([(cons-int x xs) (+ 1 (size-intlist xs))]
         [(nil-intlist) 0])))

(: main (-> () unit))
(define main ()
    (let
        ([inst_l (nil-intlist)]
         [inst_r (read)]
         [data_l (nil-intlist)]
         [data_r (nil-intlist)])
        (run inst_l inst_r data_l data_r)))

(: read (-> () intlist))
(define read ()
    (let
        ([i (in)])
        (if (=i i 33)
            (nil-intlist)
            (cons-int i (read)))))

(: run (-> (intlist intlist intlist intlist) unit))
(define run (inst_l inst_r data_l data_r)
    (case inst_r
        ([(nil-intlist)
            (begin
                (print-intlist inst_l)
                (print-intlist (nil-intlist))
                (print-intlist data_l)
                (print-intlist data_r)
                unit)]
         [(cons-int i inst_r)

            ;;;;;;;
            ;; > ;;
            ;;;;;;;
            (if (=i i 62) ; >
                (begin
                (print-sym '>')
                (print-newline)
                (case data_r
                    ([(nil-intlist)
                        (run
                            (cons-int i inst_l)
                            inst_r
                            (cons-int 0 data_l)
                            (nil-intlist))]
                     [(cons-int d data_r)
                         (run
                            (cons-int i inst_l)
                            inst_r
                            (cons-int d data_l)
                            data_r)])))

            ;;;;;;;
            ;; < ;;
            ;;;;;;;
            (if (=i i 60) ; <
                (begin
                (print-sym '<')
                (print-newline)
                (case data_l
                    ([(nil-intlist)
                        (run
                            (cons-int i inst_l)
                            inst_r
                            (nil-intlist)
                            (cons-int 0 data_r))]
                     [(cons-int d data_l)
                         (run
                            (cons-int i inst_l)
                            inst_r
                            data_l
                            (cons-int d data_r))])))

            ;;;;;;;
            ;; + ;;
            ;;;;;;;
            (if (=i i 43) ; +
                (begin
                (print-sym '+')
                (print-newline)
                (case data_r
                    ([(nil-intlist)
                        (run
                            (cons-int i inst_l)
                            inst_r
                            data_l
                            (cons-int 1 (nil-intlist)))]
                     [(cons-int d data_r)
                         (run
                            (cons-int i inst_l)
                            inst_r
                            data_l
                            (cons-int (+ d 1) data_r))])))

            ;;;;;;;
            ;; - ;;
            ;;;;;;;
            (if (=i i 45) ; -
                (begin
                (print-sym '-')
                (print-newline)
                (case data_r
                    ([(nil-intlist)
                        (run
                            (cons-int i inst_l)
                            inst_r
                            data_l
                            (cons-int -1 (nil-intlist)))]
                     [(cons-int d data_r)
                         (run
                            (cons-int i inst_l)
                            inst_r
                            data_l
                            (cons-int (- d 1) data_r))])))

            ;;;;;;;
            ;; . ;;
            ;;;;;;;
            (if (=i i 46) ; .
                (begin
                (print-sym '.')
                (print-newline)
                (case data_r
                    ([(nil-intlist)
                        (begin
                            (print-ascii 0)
                            (run
                                (cons-int i inst_l)
                                inst_r
                                data_l
                                (nil-intlist)))]
                     [(cons-int d data_r)
                        (begin
                            (print-ascii d)
                            (run
                                (cons-int i inst_l)
                                inst_r
                                data_l
                                (cons-int d data_r)))])))

            ;;;;;;;
            ;; , ;;
            ;;;;;;;
            (if (=i i 44) ; ,
                (begin
                (print-sym ',')
                (print-newline)
                (case data_r
                    ([(nil-intlist)
                        (run
                            (cons-int i inst_l)
                            inst_r
                            data_l
                            (cons-int (in) (nil-intlist)))]
                     [(cons-int d data_r)
                         (run
                            (cons-int i inst_l)
                            inst_r
                            data_l
                            (cons-int (in) data_r))])))

            ;;;;;;;
            ;; [ ;;
            ;;;;;;;
            (if (=i i 91) ; [
                (begin
                (print-sym '[')
                (print-newline)
                (case data_r
                    ([(nil-intlist)
                        (bracket_l
                            (cons-int i inst_l)
                            inst_r
                            data_l
                            (nil-intlist)
                            0)]
                     [(cons-int d data_r)
                        (if (=i d 0)
                            (bracket_l
                                (cons-int i inst_l)
                                inst_r
                                data_l
                                (cons-int d data_r)
                                0)
                            (run
                                (cons-int i inst_l)
                                inst_r
                                data_l
                                (cons-int d data_r)))])))

            ;;;;;;;
            ;; ] ;;
            ;;;;;;;
            (if (=i i 93) ; ]
                (begin
                (print-sym ']')
                (case data_r
                    ([(nil-intlist)
                        (begin
                        (print-int 0)
                        (print-newline)
                        (run
                            (cons-int i inst_l)
                            inst_r
                            data_l
                            (nil-intlist)))]
                     [(cons-int d data_r)
                        (begin
                        (print-int d)
                        (print-newline)
                        (if (not (=i d 0))
                            (bracket_r
                                inst_l
                                (cons-int i inst_r)
                                data_l
                                (cons-int d data_r)
                                0)
                            (run
                                (cons-int i inst_l)
                                inst_r
                                data_l
                                (cons-int d data_r))))])))
            ;; else
                (run
                    (cons-int i inst_l)
                    inst_r
                    data_l
                    data_r)))))))))])))

(: bracket_l (-> (intlist intlist intlist intlist int) unit))
(define bracket_l (inst_l inst_r data_l data_r depth)
    (case inst_r
        ([(nil-intlist)
            unit]
         [(cons-int i inst_r)
            (if (=i i 91)
                (bracket_l
                    (cons-int i inst_l)
                    inst_r
                    data_l
                    data_r
                    (+ depth 1))
            (if (=i i 93)
                (if (=i depth 0)
                    (run
                        (cons-int i inst_l)
                        inst_r
                        data_l
                        data_r)
                    (bracket_l
                        (cons-int i inst_l)
                        inst_r
                        data_l
                        data_r
                        (- depth 1)))
            ;; else
                (bracket_l
                    (cons-int i inst_l)
                    inst_r
                    data_l
                    data_r
                    depth)))])))

(: bracket_r (-> (intlist intlist intlist intlist int) unit))
(define bracket_r (inst_l inst_r data_l data_r depth)
    (case inst_l
        ([(nil-intlist)
            unit]
         [(cons-int i inst_l)
            (if (=i i 93)
                (bracket_r
                    inst_l
                    (cons-int i inst_r)
                    data_l
                    data_r
                    (+ 1 depth))
            (if (=i i 91)
                (if (=i depth 0)
                    (run
                        (cons-int i inst_l)
                        inst_r
                        data_l
                        data_r)
                    (bracket_r
                        inst_l
                        (cons-int i inst_r)
                        data_l
                        data_r
                        (- depth 1)))
            ;; else
                (bracket_r
                    inst_l
                    (cons-int i inst_r)
                    data_l
                    data_r
                    depth)))])))
