#lang racket/base

;; Representation of "objects" --- these describe the
;; part of an environment that an expression accesses
;;
;; See "Logical Types for Untyped Languages" pg.3

(require "../utils/utils.rkt"
         racket/match
         "rep-utils.rkt"
         "core-rep.rkt"
         "free-variance.rkt"
         (env mvar-env)
         (contract-req))

(provide -id-path name-ref=?)

(def-pathelem CarPE () [#:singleton -car])
(def-pathelem CdrPE () [#:singleton -cdr])
(def-pathelem SyntaxPE () [#:singleton -syntax-e])
(def-pathelem ForcePE () [#:singleton -force])
(def-pathelem FieldPE () [#:singleton -field])

(def-pathelem StructPE ([t Type?] [idx natural-number/c])
  [#:frees (f) (f t)]
  [#:fmap (f) (make-StructPE (f t) idx)]
  [#:for-each (f) (f t)])

(def-object Path ([elems (listof PathElem?)] [name name-ref/c])
  [#:frees (f)  (combine-frees (map f elems))]
  [#:fmap (f) (make-Path (map f elems) name)]
  [#:for-each (f) (for-each f elems)]
  [#:custom-constructor
   (cond
     [(identifier? name)
      (if (is-var-mutated? name)
          -empty-obj
          (let ([name (normalize-id name)])
            (intern-double-ref!
             path-intern-table
             name elems #:construct (make-Path elems name))))]
     [else (intern-double-ref!
            path-intern-table
            name elems #:construct (make-Path elems name))])])

(define path-intern-table (make-weak-hash))

(define (-id-path name) (make-Path null name))
