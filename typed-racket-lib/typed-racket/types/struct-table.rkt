#lang racket/base

(require syntax/id-table racket/match
         racket/syntax
         "../utils/utils.rkt"
         (prefix-in c: (contract-req))
         (rep type-rep prop-rep object-rep)
         (utils tc-utils)
         (env env-utils)
         (types abbrev))

;; mapping of identifiers to (list pe mutator? mutable?) where
;; pe : PathElem is the path element for the field corresponding
;;      to this function
;; mutator? : Bool indicates if this id is a mutator
;;            (as opposed to an accessor)
;; mutable? : Bool indicating if this field is mutable
(define struct-fn-table (make-free-id-table))


(define (add-struct-accessor-fn! id pe mutable-field?)
  (free-id-table-set! struct-fn-table id (list pe #f mutable-field?)))

(define (add-struct-mutator-fn! id pe)
  (free-id-table-set! struct-fn-table id (list pe #t #t)))

(define (struct-accessor? id)
  (match (free-id-table-ref struct-fn-table id #f)
    [(list pe #f _) pe]
    [_ #f]))

(define (struct-mutator? id)
  (match (free-id-table-ref struct-fn-table id #f)
    [(list pe #t _) pe]
    [_ #f]))

(define (struct-fn-idx id)
  (match (free-id-table-ref struct-fn-table id #f)
    [(list (StructPE: _ idx) _ _) idx]
    [(list (PrefabPE: _ idx) _ _) idx]
    [_ (int-err (format "no struct fn table entry for ~a" (syntax->datum id)))]))

(define (immutable-struct-field-accessor? id)
  (match (free-id-table-ref struct-fn-table id #f)
    [(list pe #f #f) #t]
    [_ #f]))

(define (struct-fn-table-map f)
  (for/list ([(k v) (in-sorted-dict struct-fn-table id<)])
    (f k v)))

(define (id-for-struct-pe path-elem=?)
  (for*/or ([(id entry) (in-free-id-table struct-fn-table)]
            [pe (in-value (car entry))]
            #:when (path-elem=? pe))
    id))


(provide/cond-contract
 [add-struct-accessor-fn! (identifier? (c:or/c StructPE? PrefabPE?) boolean? . c:-> . c:any/c)]
 [add-struct-mutator-fn! (identifier? (c:or/c StructPE? PrefabPE?) . c:-> . c:any/c)]
 [struct-accessor? (identifier? . c:-> . (c:or/c #f StructPE? PrefabPE?))]
 [struct-mutator? (identifier? . c:-> . (c:or/c #f StructPE? PrefabPE?))]
 [struct-fn-idx (identifier? . c:-> . exact-integer?)]
 [immutable-struct-field-accessor? (identifier? . c:-> . boolean?)]
 [id-for-struct-pe (c:-> (c:-> PathElem? boolean?) (c:or/c identifier? #f))]
 [struct-fn-table-map (c:-> (c:-> identifier?
                                  (c:list/c (c:or/c StructPE? PrefabPE?) boolean? boolean?)
                                  c:any/c)
                            (c:listof c:any/c))])