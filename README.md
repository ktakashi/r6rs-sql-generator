SQL generator for R6RS Scheme
=============================

Motivation
----------

Writing SQL in Scheme script is pain in the A$$. It might be good if we
can write it in S-expression.

Usage
-----

```scheme

#!r6rs
(import (rnrs)
	(text sql generator))

(sexp->sql-string '(select * from a-table))
;; -> " select * from a-table "

```

Supported SQL statements
------------------------

TBD

`select`

`from`

`where`

and some expressions


Adding other queries
--------------------

TBD

`define-sql-statement`

`define-sql-clause`

`define-sql-expression`
