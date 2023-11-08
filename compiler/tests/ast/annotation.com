(: add-or-sub (-> (bool) (-> (int int) int)))
(: funnyfun (-> ((-> (very odd) function) (-> () (-> (words) int))) (-> (final) (-> (sike) real))))
