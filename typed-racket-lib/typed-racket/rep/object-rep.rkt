#lang racket/base

;; Representation of "objects" --- these describe the
;; part of an environment that an expression accesses
;;
;; See "Logical Types for Untyped Languages" pg.3

(require "../utils/utils.rkt"
         "rep-utils.rkt"
         "core-rep.rkt"
         "free-variance.rkt"
         (env mvar-env)
         (contract-req))

(provide -id-path)

(def-pathelem CarPE () #:base)
(def-pathelem CdrPE () #:base)
(def-pathelem SyntaxPE () #:base)
(def-pathelem ForcePE () #:base)
;; t is always a Name (can't put that into the contract b/c of circularity)
(def-pathelem StructPE ([t Type?] [idx natural-number/c])
  [#:intern-key (cons (Rep-seq t) idx)]
  [#:frees (f) (f t)]
  [#:fold (f) (make-StructPE (f t) idx)]
  [#:walk (f) (f t)])
(def-pathelem FieldPE () #:base)

(def-object Path ([elems (listof PathElem?)] [name name-ref/c])
  [#:intern-key (cons (hash-name name) (map Rep-seq elems))]
  [#:frees (f)  (combine-frees (map f elems))]
  [#:fold (f) (make-Path (map f elems) name)]
  [#:walk (f) (for-each f elems)])

(define (-id-path id)
  (cond
    [(identifier? id)
     (if (is-var-mutated? id)
         (make-Empty)
         (make-Path null id))]
    [else
     (make-Path null id)]))
