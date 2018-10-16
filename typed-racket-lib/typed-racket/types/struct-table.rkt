#lang racket/base

(require syntax/private/id-table racket/match
         racket/syntax
         "../utils/utils.rkt"
         (prefix-in c: (contract-req))
         (rep type-rep prop-rep object-rep)
         (utils tc-utils)
         (env env-utils)
         (types abbrev))


;; struct-type : what type is this an accessor for?
;; info : bitfield with field info
;; bit 0 : is this a mutator?
;; bit 1 : field mutability
;; bit 2 : is this for a prefab struct?
;; bits 3-rest : field index
;; NOTE: since `info` is a bitfield, `struct-field-metadata` instances
;; should generally be created with `mk-field-metadata` and not manually.
(struct struct-field-metadata (struct-type info) #:prefab)

;; mk-field-metadata
;;
;; constructs a `struct-field-metadata` struct, recording the type
;; for which a field accessor/mutator is valid for, and packing
;; other information into a bitfield (see struct-field-metadata above).
(define (mk-field-metadata struct-type field-index mutator? mutable? prefab?)
  (struct-field-metadata
   struct-type
   (bitwise-ior (if mutator? #b001 #b000)
                (if mutable? #b010 #b000)
                (if prefab?  #b100 #b000)
                (arithmetic-shift field-index 3))))

(define (struct-field-accessor? metadata) (not (struct-field-mutator? metadata)))
(define (struct-field-mutator? metadata) (bitwise-bit-set? (struct-field-metadata-info metadata)  0))
(define (struct-field-mutable? metadata) (bitwise-bit-set? (struct-field-metadata-info metadata)  1))
(define (struct-field-prefab? metadata)  (bitwise-bit-set? (struct-field-metadata-info metadata)  2))
(define (struct-field-index metadata)    (arithmetic-shift (struct-field-metadata-info metadata) -3))

;; mapping from identifiers (i.e. struct accessor/mutators) to
;; information about the values they operate on, etc.
;; See struct-field-metadata above.
(define struct-fn-table (make-free-id-table))


;; add-struct-field-metadata!
;;
;; Records that the identifier `fn-id` is a struct
;; accessor/mutator with `metadata` being a struct-field-metadata
;; (see above) describing properties of that accessor/mutator.
(define (add-struct-field-metadata! fn-id metadata)
  (free-id-table-set! struct-fn-table fn-id metadata))

;; add-struct-accessor-fn!
;;
;; Records that identifier `fn-id` as an accessor for
;; the `idx`th field of struct values of type `type`.
;; mutable-field? - a boolean describing whether the field is mutable.
;; prefab? - a boolean describing if this is a prefab struct
(define (add-struct-accessor-fn! fn-id type idx mutable-field? prefab?)
  (free-id-table-set! struct-fn-table fn-id (mk-field-metadata type idx #f mutable-field? prefab?)))

;; add-struct-mutator-fn!
;;
;; Records that identifier `fn-id` as a mutator for
;; the `idx`th field of struct values of type `type`.
;; prefab? - a boolean describing if this is a prefab struct
(define (add-struct-mutator-fn! fn-id type idx prefab?)
  (free-id-table-set! struct-fn-table fn-id (mk-field-metadata type idx #t #t prefab?)))


(define (struct-accessor? id)
  (match (free-id-table-ref struct-fn-table id #f)
    [(? struct-field-metadata? entry)
     (and (not (struct-field-mutator? entry))
          (struct-field-index entry))]
    [_ #f]))

(define (struct-mutator? id)
  (match (free-id-table-ref struct-fn-table id #f)
    [(? struct-field-metadata? entry)
     (and (struct-field-mutator? entry)
          (struct-field-index entry))]
    [_ #f]))

(define (immutable-struct-field-accessor? id)
  (match (free-id-table-ref struct-fn-table id #f)
    [(? struct-field-metadata? entry)
     (and (not (struct-field-mutable? entry))
          (struct-field-index entry))]
    [_ #f]))

(define (prefab-struct-field-operator? id)
  (match (free-id-table-ref struct-fn-table id #f)
    [(? struct-field-metadata? entry) (struct-field-prefab? entry)]
    [_ #f]))


(define (struct-fn-table-map f)
  (for/list ([(k v) (in-sorted-free-id-table struct-fn-table)])
    (f k v)))


;; find-struct-accessor-id
;;
;; takes a predicate `pred` of type (-> Type? natural? boolean?)
;; and searches for a struct-accessor identifier for whom
;; `(pred type index)` produces a non-#f value, where
;; `type` and `index` are the corresponding info in the recorded
;; struct metadata (see `struct-field-metadata` above)
(define (find-struct-accessor-id pred)
  (for*/or ([(id entry) (in-free-id-table struct-fn-table)]
            #:when (struct-field-accessor? entry)
            [type (in-value (struct-field-metadata-struct-type entry))]
            [index (in-value (struct-field-index entry))]
            #:when (pred type index))
    id))


(provide struct-field-metadata)

(provide/cond-contract
 [add-struct-accessor-fn! (identifier? Type? exact-nonnegative-integer? boolean? boolean? . c:-> . c:any/c)]
 [add-struct-mutator-fn! (identifier? Type? exact-nonnegative-integer? boolean? . c:-> . c:any/c)]
 [add-struct-field-metadata! (identifier? struct-field-metadata? . c:-> . c:any/c)]
 [struct-accessor? (identifier? . c:-> . (c:or/c #f exact-nonnegative-integer?))]
 [struct-mutator? (identifier? . c:-> . (c:or/c #f exact-nonnegative-integer?))]
 [prefab-struct-field-operator? (identifier? . c:-> . c:any/c)]
 [immutable-struct-field-accessor? (identifier? . c:-> . exact-nonnegative-integer?)]
 [find-struct-accessor-id (c:-> (c:-> Type? exact-nonnegative-integer? boolean?) (c:or/c identifier? #f))]
 [struct-fn-table-map (c:-> (c:-> identifier? struct-field-metadata? c:any/c)
                            (c:listof c:any/c))])