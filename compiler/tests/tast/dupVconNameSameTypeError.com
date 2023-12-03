(datatype int-list                   
    ([cons-int (int int-list)]
     [cons-int (int)] ;; same variant-constructor name, different number of arguments
     [nil-intlist ()]))