SQL generator for R6RS Scheme
=============================

Motivation
----------

Writing SQL in Scheme script is pain in the A$$. It might be good if we
can write it in S-expression.

Requirements
------------

This library requires Andrew Wright's/Alex Shinn's  pattern match library.
It must be named `(match)`.

Currently it's tested on Sagittarius and Mosh.


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

