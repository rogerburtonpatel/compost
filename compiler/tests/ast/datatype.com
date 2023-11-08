(datatype cons ([car ((-> (int cons) int))] [cdr ((-> (int cons) cons))]))
