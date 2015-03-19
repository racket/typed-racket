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

(provide path-type unpath-type)

(define-custom-set-types type-set
  #:elem? Type?
  type-equal?
  Rep-seq)

(define empty-type-set (make-immutable-type-set))

;; returns the result of following a path into a type
;; (Listof PathElem) Type -> Type
;; Ex. '(CarPE) (Pair α β) -> α
;; resolved is the set of resolved types so far at a particular
;; path - it ensures we are making progress, that we do not
;; continue unfolding types infinitely while not progressing.
;; It is intentionally reset each time we decrease the
;; paths size on a recursive call, and maintained/extended
;; when the path does not decrease on a recursive call.
(define/cond-contract (path-type path t [resolved empty-type-set])
  (->* ((listof PathElem?) 
        Type/c) 
       (immutable-type-set?)
       Type/c)
  
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
    
    ;; types which need resolving
    [((? needs-resolving?) _) #:when (not (set-member? resolved t))
     (path-type path (resolve-once t) (set-add resolved t))]
    
    ;; type/path mismatch =(
    [(_ _) Err]))


;; takes a path and a type and builds up the type from 'unwrapping'
;; the path. Ex: (car cdr) String --> (Pairof (Pairof Any String) Any)
(define/cond-contract (unpath-type path t fail-type)
  (-> (listof PathElem?) Type/c Type/c
       Type/c)
  
  (match path
    ;; empty path
    [(list) t]
    
    ;; pair ops
    [(list rst ... (CarPE:))
     (unpath-type rst (-pair t Univ) fail-type)]
    [(list rst ... (CdrPE:))
     (unpath-type rst (-pair Univ t) fail-type)]
    
    ;; syntax ops
    [(list rst ... (SyntaxPE:))
     (unpath-type rst (-Syntax Univ) fail-type)]
    
    ;; promise op
    [(list rst ... (ForcePE:))
     (unpath-type rst (-Promise Univ) fail-type)]
    
    ;; struct ops
    #;[(list rst ... (StructPE: (? (λ (s) (subtype t s)) s) idx))
       ;TODO(amk) punt for now! support later?
       (int-error "nope - not supported! Hey, who uncommented this!?")]
    
    ;; default to Bottom (since this function supports claims about negative types
    ;; (at least it does right now...)
    [_ fail-type]))

