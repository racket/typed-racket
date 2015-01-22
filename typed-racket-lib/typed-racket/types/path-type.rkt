#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/set
         (contract-req)
         (rep object-rep type-rep)
         (utils tc-utils)
         (typecheck renamer)
         (types subtype resolve union)
         (except-in (types utils abbrev kw-types) -> ->* one-of/c))

(require-for-cond-contract (rep rep-utils))

(provide/cond-contract
 [path-type ((listof PathElem?) Type/c . -> . Type/c)])


;; returns the result of following a path into a type
;; (Listof PathElem) Type -> Type
;; Ex. '(CarPE) (Pair α β) -> α
;; resolved is the set of resolved types so far at a particular
;; path - it ensures we are making progress, that we do not
;; continue unfolding types infinitely while not progressing.
;; It is intentionally reset each time we decrease the
;; paths size on a recursive call, and maintained/extended
;; when the path does not decrease on a recursive call.
(define (path-type path t [resolved (set)])
  (match* (t path)
    ;; empty path
    [(t (list)) t]
    
    ;; pair ops
    [((Pair: t s) (list rst ... (CarPE:)))
     (path-type rst t)]
    [((Pair: t s) (list rst ... (CdrPE:)))
     (path-type rst s)]

    ;; syntax ops
    [((Syntax: t) (list rst ... (SyntaxPE:)))
     (path-type rst t)]

    ;; promise op
    [((Promise: t) (list rst ... (ForcePE:)))
     (path-type rst t)]

    ;; struct ops
    [((Struct: nm par flds proc poly pred)
      (list rst ... (StructPE: (? (λ (s) (subtype t s)) s) idx)))
     (match-let ([(fld: ft _ _) (list-ref flds idx)])
       (path-type rst ft))]
    
    [((Union: ts) _)
     (apply Un (map (λ (t) (path-type path t resolved)) ts))]
    
    ;; paths into polymorphic types
    [((Poly: _ body-t) _) (path-type path body-t resolved)]
    [((PolyDots: _ body-t) _) (path-type path body-t resolved)]
    [((PolyRow: _ _ body-t) _) (path-type path body-t resolved)]
    
    ;; for private fields in classes
    [((Function: (list (arr: doms (Values: (list (Result: rng _ _))) _ _ _)))
      (list rst ... (FieldPE:)))
     (path-type rst rng)]

    ;; types which need resolving
    [((? needs-resolving?) _) #:when (not (set-member? resolved t))
     (path-type path (resolve-once t) (set-add resolved t))]
    
    ;; type/path mismatch =(
    [(_ _) Err]))

