#lang racket/base

(require syntax/id-table racket/match
         racket/syntax
         "../utils/utils.rkt"
         (prefix-in c: (contract-req))
         (rep type-rep prop-rep object-rep)
         (utils tc-utils)
         (env env-utils)
         (types abbrev))


;; struct-type : what type is this an accessor for?
;; field-index : what is the (absolute) field-index for this field?
;;             (i.e. the `i` where `(unsafe-struct-ref s i)` would return
;;                   this field)
;; mutator? : #t if this is a mutator, #f if it is an accessor
;; mutable? : #t if this field is mutable, #f if it is immutable
(struct struct-field-entry (struct-type field-index mutator? mutable?) #:prefab)

(define struct-fn-table (make-free-id-table))


(define (add-struct-accessor-fn! fn-id type idx mutable-field?)
  (free-id-table-set! struct-fn-table fn-id (struct-field-entry type idx #f mutable-field?)))

(define (add-struct-mutator-fn! fn-id type idx)
  (free-id-table-set! struct-fn-table fn-id (struct-field-entry type idx #t #t)))

(define (struct-accessor? id)
  (match (free-id-table-ref struct-fn-table id #f)
    [(struct-field-entry _ idx #f _) idx]
    [_ #f]))

(define (struct-mutator? id)
  (match (free-id-table-ref struct-fn-table id #f)
    [(struct-field-entry _ idx #t _) idx]
    [_ #f]))

(define (immutable-struct-field-accessor? id)
  (match (free-id-table-ref struct-fn-table id #f)
    [(struct-field-entry _ idx #t #t) idx]
    [_ #f]))

(define (struct-fn-table-map f)
  (for/list ([(k v) (in-sorted-dict struct-fn-table id<)])
    (f k v)))

(define (id-for-struct-pe type/idx=?)
  (for*/or ([(id entry) (in-free-id-table struct-fn-table)]
            [type (in-value (struct-field-entry-struct-type entry))]
            [idx (in-value (struct-field-entry-field-index entry))]
            #:when (type/idx=? type idx))
    id))


(provide struct-field-entry)

(provide/cond-contract
 [add-struct-accessor-fn! (identifier? Type? exact-nonnegative-integer? boolean? . c:-> . c:any/c)]
 [add-struct-mutator-fn! (identifier? Type? exact-nonnegative-integer? . c:-> . c:any/c)]
 [struct-accessor? (identifier? . c:-> . (c:or/c #f exact-nonnegative-integer?))]
 [struct-mutator? (identifier? . c:-> . (c:or/c #f exact-nonnegative-integer?))]
 [immutable-struct-field-accessor? (identifier? . c:-> . exact-nonnegative-integer?)]
 [id-for-struct-pe (c:-> (c:-> Type? exact-nonnegative-integer? boolean?) (c:or/c identifier? #f))]
 [struct-fn-table-map (c:-> (c:-> identifier? struct-field-entry? c:any/c)
                            (c:listof c:any/c))])