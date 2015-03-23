;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sql/generator.sls - A SQL generator 
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(library (text sql generator)
    (export sexp->sql-string
	    
	    ;; for customise
	    define-sql-statement
	    define-sql-clause
	    define-sql-expression
	    )
    (import (for (rnrs) run expand)
	    (match))

(define *command-handler* (make-eq-hashtable))
(define *sql-type* (make-eq-hashtable))

(define-syntax define-sql-handler
  (lambda (x)
    (define (->name c s)
      (string->symbol
       (string-append (symbol->string (syntax->datum c))
		      "-"
		      (symbol->string (syntax->datum s)))))
    (syntax-case x ()
      ((k state (command ssql ctx out) body ...)
       (with-syntax ((name (datum->syntax #'k (->name #'command #'state))))
	 #'(define name
	     (let ((p (lambda (ssql ctx out) body ...)))
	       (hashtable-set! *command-handler* 'command p)
	       (hashtable-set! *sql-type* 'command 'state)
	       p)))))))

(define-syntax define-sql-statement
  (syntax-rules ()
    ((_ (command ssql ctx out) body ...)
     (define-sql-handler statement (command ssql ctx out) body ...))))
(define-syntax define-sql-clause
  (syntax-rules ()
    ((_ (command ssql ctx out) body ...)
     (define-sql-handler clause (command ssql ctx out) body ...))))
(define-syntax define-sql-expression
  (syntax-rules ()
    ((_ (command ssql ctx out) body ...)
     (define-sql-handler clause (command ssql ctx out) body ...))))
(define-syntax define-sql-binary-expression
  (syntax-rules ()
    ((_ command)
     (define-sql-expression (command ssql ctx out)
       (match ssql
	 ((_ a b)
	  (display " " out)
	  ;; for sub query
	  (display (sexp->sql-string a) out)
	  (display " " out)
	  (display 'command out)
	  (display " " out)
	  (display (sexp->sql-string b) out)))))))
(define-syntax define-sql-unary-expression
  (syntax-rules ()
    ((_ command)
     (define-sql-expression (command ssql ctx out)
       (display " " out)
       (display 'command out)
       (display " " out)
       (display (sexp->sql-string (cadr ssql) out))))))

;; util
(define (write/separator sexp sep out)
  (let loop ((sexp sexp) (first #t))
    (unless (null? sexp)
      (unless first (display sep out))
      (let ((e (car sexp)))
	(cond ((and (pair? e)
		    (eq? (hashtable-ref *sql-type* (car e) #f) 'statement))
	       (display "(" out)
	       (display (sexp->sql-string e) out)
	       (display ")" out))
	      (else (display (sexp->sql-string e) out))))
      (loop (cdr sexp) #f))))
(define (write/comma sexp out)
  (write/separator sexp ", " out))

;; TODO escape char as parameter?
(define (escape-sql-string str)
  (call-with-string-output-port
   (lambda (out)
     (display #\' out)
     (string-for-each
      (lambda (c)
	(when (char=? c #\') (display #\' out))
	(display c out))
     str)
     (display #\' out))))

;; (select columns from-clause rest ...)
(define-sql-statement (select ssql ctx out)
  (match ssql
    ;; syntax sugar for common use cases
    ((_ columns 'from table rest ...)
     (select-statement `(select ,columns (from ,table) ,@rest) ctx out))
    ((_ '* ('from clause ...) rest ...)
     (display " select * " out) 
     (display (sexp->sql-string (cons 'from clause)) out)
     (for-each (lambda (s) (display (sexp->sql-string s) out)) rest))
    ((_ (? pair? columns) ('from clause ...) rest ...)
     (display " select " out) 
     (write/comma columns out)
     (display (sexp->sql-string (cons 'from clause)) out)
     (for-each (lambda (s) (display (sexp->sql-string s) out)) rest))
    ;; PostgreSQL allow SQL not to have 'from' clause
    ;; this is alternative of 'dual' on Oracle so support it.
    ;; NB: not standard SQL but for convenience for me.
    ((_ (? pair? columns))
     (display " select " out) 
     (write/comma columns out))))

(define-sql-clause (from ssql ctx out)
  (display " from " out)
  (write/comma (cdr ssql) out))

(define-sql-clause (where ssql ctx out)
  (match ssql
    ((_ condition)
     (display " where " out)
     (display (sexp->sql-string condition) out))))

(define-sql-clause (order-by ssql ctx out)
  (display " order by " out)
  (write/comma (cdr ssql) out))

(define-sql-expression (desc ssql ctx out)
  (match ssql
    ((_ column) 
     (display (sexp->sql-string column) out)
     (display " desc" out))))
(define-sql-expression (asc ssql ctx out)
  (match ssql
    ((_ column)
     (display (sexp->sql-string column) out)
     (display " asc" out))))

(define-sql-expression (or ssql ctx out)
  (match ssql
    ((_ expr ...)
     (display "(" out)
     (write/separator expr " or " out)
     (display ")" out))))
(define-sql-expression (and ssql ctx out)
  (match ssql
    ((_ expr ...)
     (display "(" out)
     (write/separator expr " and " out)
     (display ")" out))))

;; ugly, should we make (null? ! expr) for IS NOT NULL?
(define-sql-expression (null? ssql ctx out)
  (match ssql
    ((_ expr)
     (display (sexp->sql-string expr) out)
     (display " is null" out))))
(define-sql-expression (not-null? ssql ctx out)
  (match ssql
    ((_ expr)
     (display (sexp->sql-string expr) out)
     (display " is not null" out))))

(define-sql-expression (in ssql ctx out)
  (match ssql
    ((_ expr ...)
     (display "in (" out)
     (write/comma expr out)
     (display ")" out))))

(define-sql-expression (between ssql ctx out)
  (match ssql
    ((_ a b)
     (display "between " out)
     (display (sexp->sql-string a) out)
     (display " and " out)
     (display (sexp->sql-string b) out))))

(define-sql-unary-expression not)

(define-sql-binary-expression as)
(define-sql-binary-expression >=)
(define-sql-binary-expression <=)
(define-sql-binary-expression >)
(define-sql-binary-expression <)
(define-sql-binary-expression =)

;; alter-table
(define-sql-statement (alter-table ssql ctx out)
  (match ssql
    ((_ name action1 actions ...)
     (display " alter table " out)
     (display name out) ;; we just emit the name
     ;; TODO proper check
     (display " " out)
     (display (sexp->sql-string action1) out)
     (for-each (lambda (e)
		 (display " " out)
		 (display (sexp->sql-string e) out)) actions))))

(define (sexp->sql-string ssql)
  (cond ((pair? ssql)
	 (cond ((hashtable-ref *command-handler* (car ssql) #f)
	       => (lambda (handler)
		    (call-with-string-output-port
		     (lambda (out)
		       (let ((ctx (hashtable-ref *sql-type* (car ssql) #f)))
			 (handler ssql ctx out))))))
	       (else
		;; just put it so (a > b) would be "a > b"
		;; not sure how useful it is but better than
		;; throwing an error :)
		(call-with-string-output-port
		 (lambda (out) (write/separator ssql " " out))))))
	((number? ssql) (number->string ssql))
	((string? ssql) (escape-sql-string ssql))
	((symbol? ssql) (symbol->string ssql))
	(else (error 'sexp->sql-string "unsupported type"  ssql))))
)
