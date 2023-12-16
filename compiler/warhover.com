(val jackson-str 'welcome to watch2gether 

start a lobby or join by url
')

(: main (-> () unit))
(define main ()
    (print-sym jackson-str))


