#lang racket/base

;; Representation of "objects" --- these describe the
;; part of an environment that an expression accesses
;;
;; See "Logical Types for Untyped Languages" pg.3

(require "rep-utils.rkt" "free-variance.rkt" "prop-rep.rkt" "../utils/utils.rkt" (contract-req))
(provide object-equal?)

(def-pathelem CarPE () [#:fold-rhs #:base])
(def-pathelem CdrPE () [#:fold-rhs #:base])
(def-pathelem SyntaxPE () [#:fold-rhs #:base])
(def-pathelem ForcePE () [#:fold-rhs #:base])
;; t is always a Name (can't put that into the contract b/c of circularity)
(def-pathelem StructPE ([t Type?] [idx natural-number/c])
  [#:frees (λ (f) (f t))]
  [#:fold-rhs (*StructPE (type-rec-id t) idx)])
(def-pathelem FieldPE () [#:fold-rhs #:base])

(def-object Empty () [#:fold-rhs #:base])

(def-object Path ([p (listof PathElem?)] [v name-ref/c])
  [#:intern (list (map Rep-seq p) (hash-name v))]
  [#:frees (λ (f) (combine-frees (map f p)))]
  [#:fold-rhs (*Path (map pathelem-rec-id p) v)])

(define (object-equal? o1 o2) (= (Rep-seq o1) (Rep-seq o2)))
