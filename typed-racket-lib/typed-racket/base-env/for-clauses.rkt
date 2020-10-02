#lang racket/base

(require syntax/parse/pre
         (for-template (only-in racket/base quote))
         "annotate-classes.rkt")

(provide (all-defined-out))

(define-syntax-class no-opt-expr
  (pattern e0:expr
           #:with e (syntax-property #'e0 'for:no-implicit-optimization #t)))

;; intersperses "#:when #t" clauses to emulate the for* variants' semantics
(define-splicing-syntax-class for-clause
  #:description "for clause"
  ;; single-valued seq-expr
  (pattern (~and c (var:optionally-annotated-name seq-expr:no-opt-expr))
           #:with (expand ...) #`(#,(syntax/loc #'c
                                      (var.ann-name seq-expr.e)))
           #:with (expand* ...) #'(expand ... #:when '#t))
  ;; multi-valued seq-expr
  (pattern (~and c ((v:optionally-annotated-formal ...) seq-expr:no-opt-expr))
           #:with (expand ...) (list (quasisyntax/loc #'c
                                       ((v.ann-name ...) seq-expr.e)))
           #:with (expand* ...) (list (quasisyntax/loc #'c
                                        ((v.ann-name ...) seq-expr.e))
                                      #'#:when #''#t))
  (pattern (~seq (~and kw (~or #:when #:unless #:break #:final)) guard:expr)
           #:with (expand ...) (list #'kw #'guard)
           #:with (expand* ...) #'(expand ...)))

(define-syntax-class for-clauses
  #:description "for clauses"
  #:attributes ((expand 2) (expand* 2))
  (pattern (:for-clause ...)))

(define-syntax-class accumulator-binding
  #:description "accumumulator binding"
  #:attributes (ann-name init ty)
  (pattern (:optionally-annotated-name init:expr)))

(define-splicing-syntax-class result-clause
  #:description "result clause"
  (pattern (~seq #:result result-expr:expr)))

(define-syntax-class accumulator-bindings
  #:description "accumumulator bindings"
  #:attributes ((ann-name 1) (init 1) (ty 1) result)
  (pattern (:accumulator-binding ... (~optional result:result-clause))))
