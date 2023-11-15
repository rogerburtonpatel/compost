(datatype intlist 
    ([cons-int (int intlist)]
     [nil-intlist ()]))

(datatype direction 
    ([left ()]
     [right ()]
     [up ()]
     [down ()]))

(datatype bst 
    ([buildbst (int bst bst)]
     [nil-bst ()]))    

(datatype multilist 
    ([cons-multi-int (int multilist)]
     [cons-multi-bool (bool multilist)]
     [nil-multilist ()]))

(datatype twointlist 
    ([build (intlist intlist)]))

(: main (-> () unit))
(define main ()
    (let 
        ([x (cons-int 3 (nil-intlist))]
         [y (dup x)])
         unit))
