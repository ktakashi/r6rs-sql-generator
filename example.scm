(import (rnrs)
	(text sql generator))

(define (print . args) (for-each display args) (newline))

(print (sexp->sql-string '(select * from a-table)))

