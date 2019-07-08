#lang racket/base

(require "../utils/utils.rkt"
         (utils prefab)
         (rep type-rep rep-utils type-mask)
         (prefix-in c: (contract-req))
         (types abbrev subtype resolve utils)
         racket/match)


(provide overlap?)

(define (simple-datum? v)
  (or (null? v)
      (symbol? v)
      (number? v)
      (boolean? v)
      (pair? v)
      (string? v)
      (keyword? v)
      (char? v)
      (void? v)
      (eof-object? v)))

;; let's not loop forever while checking overlap
(define current-seen (make-parameter '()))
(define (seen? t1 t2)
  (for*/or ([p (in-list (current-seen))]
            [a (in-value (car p))]
            [d (in-value (cdr p))])
    (or (and (equal? t1 a)
             (equal? t2 d))
        (and (equal? t1 d)
             (equal? t2 a)))))
(define-syntax-rule (with-updated-seen t1 t2 body ...)
  (parameterize ([current-seen (cons (cons t1 t2) (current-seen))])
    body ...))

;; overlap?
;; Type Type -> Boolean
;; a conservative check to see if two types
;; have a non-empty intersection
(define/cond-contract (overlap? t1 t2)
  (c:-> Type? Type? boolean?)
  (match*/no-order
   (t1 t2)
   [(_ _) #:when (equal? t1 t2) #t]
   [(_ _) #:when (disjoint-masks? (mask t1) (mask t2)) #f]
   [(_ _) #:when (seen? t1 t2) #t]
   [((Univ:) _) #:no-order #t]
   [((or (B: _) (F: _)) _) #:no-order #t]
   [((Opaque: _) _) #:no-order #t]
   [((Name/simple: n) (Name/simple: n*)) #:when (free-identifier=? n n*) #t]
   [((Distinction: _ _ t1) t2) #:no-order (overlap? t1 t2)]
   [(t1 (or (? Name? t2)
            (? App? t2)))
    #:no-order
    (with-updated-seen t1 t2
      (let ([t2 (resolve-once t2)])
        (or (not t2) (overlap? t1 t2))))]
   [((? Mu? t1) t2)
    #:no-order
    (with-updated-seen t1 t2
      (overlap? (unfold t1) t2))]
   [((Refinement: t1 _) t2) #:no-order (overlap? t1 t2)]
   [((BaseUnion: bbits1 nbits1)
     (BaseUnion: bbits2 nbits2))
    (or (bbits-overlap? bbits1 bbits2)
        (nbits-overlap? nbits1 nbits2))]
   [((BaseUnion: bbits nbits) (Base-bits: num? bits))
    #:no-order
    (if num?
        (nbits-overlap? nbits bits)
        (bbits-overlap? bbits bits))]
   [((BaseUnion-bases: bases1) t2)
    #:no-order
    (for/or ([b1 (in-list bases1)]) (overlap? b1 t2))]
   [((Union: (BaseUnion: bbits1 nbits1) _)
     (Union: (BaseUnion: bbits2 nbits2) _))
    #:when (or (bbits-overlap? bbits1 bbits2)
               (nbits-overlap? nbits1 nbits2))
    #t]
   [((Union/set: base1 ts1 elems1) t2)
    #:no-order
    (or (hash-ref elems1 t2 #f)
        (overlap? base1 t2)
        (for/or ([t1 (in-list ts1)]) (overlap? t1 t2)))]
   ;; we ignore the possible refining prop for simplicities sake
   [((Intersection: ts _) s)
    #:no-order
    (for/and ([t (in-list ts)]) (overlap? t s))]
   [((or (Poly-unsafe: _ t1)
         (PolyDots-unsafe: _ t1))
     t2)
    #:no-order
    (overlap? t1 t2)] ;; conservative
   [((? Base?) (? Base?)) (or (subtype t1 t2) (subtype t2 t1))]
   [((? Base? t) (? Value? s)) #:no-order (subtype s t)] ;; conservative
   [((Syntax: t) (Syntax: t*)) (overlap? t t*)]
   [((Syntax: _) _) #:no-order #f]
   [((? Base?) _) #:no-order #f]
   [((Value: (? pair?)) (Pair: _ _)) #:no-order #t]
   [((Pair: a b) (Pair: a* b*)) (and (overlap? a a*)
                                     (overlap? b b*))]
   ;; lots of things are sequences, but not values where sequence? produces #f
   [((or (Sequence: _)
         (SequenceTop:))
     (Val-able: v))
    #:no-order
    (sequence? v)]
   ;; hash tables are two-valued sequences
   [((Sequence: (or (list _) (list _ _ _ ...)))
     (or (HashTableTop:)
         (Mutable-HashTable: _ _)
         (Weak-HashTable: _ _)))
    #:no-order
    #f]
   ;; these are single-valued sequences
   [((Sequence: (list _ _ _ ...))
     (or (? Pair?) (? Vector?) (VectorTop:)))
    #:no-order
    #f]
   ;; be conservative about other kinds of sequences
   [((or (Sequence: _)
         (SequenceTop:))
     _)
    #:no-order
    #t]
   ;; Values where evt? produces #f cannot be Evt
   [((Evt: _) (Val-able: v)) #:no-order (evt? v)]
   [((Pair: _ _) _) #:no-order #f]
   [((Val-able: (? simple-datum? v1))
     (Val-able: (? simple-datum? v2)))
    (equal? v1 v2)]
   [((Val-able: (? simple-datum?))
     (or (? Struct?) (? StructTop?) (? Fun?)))
    #:no-order
    #f]
   [((Val-able: (not (? hash?)))
     (or (HashTableTop:)
         (Mutable-HashTable: _ _)
         (Weak-HashTable: _ _)))
    #:no-order
    #f]
   [((Struct: n _ flds _ _ _ _)
     (Struct: n* _ flds* _ _ _ _))
    #:when (free-identifier=? n n*)
    (for/and ([f (in-list flds)] [f* (in-list flds*)])
      (match* (f f*)
        [((fld: t _ _) (fld: t* _ _)) (overlap? t t*)]))]
   [((Struct: n #f _ _ _ _ _)
     (StructTop: (Struct: n* #f _ _ _ _ _)))
    #:when (free-identifier=? n n*)
    #t]
   [((StructTop: (Struct: n* #f _ _ _ _ _))
     (Struct: n #f _ _ _ _ _))
    #:when (free-identifier=? n n*)
    #t]
   ;; n and n* must be different, so there's no overlap
   [((Struct: n #f flds _ _ _ _)
     (Struct: n* #f flds* _ _ _ _))
    #f]
   [((Struct: n #f flds _ _ _ _)
     (StructTop: (Struct: n* #f flds* _ _ _ _)))
    #f]
   [((StructTop: (Struct: n* #f flds* _ _ _ _))
     (Struct: n #f flds _ _ _ _))
    #f]
   [((and t1 (Struct: _ _ _ _ _ _ _))
     (and t2 (Struct: _ _ _ _ _ _ _)))
    (or (subtype t1 t2) (subtype t2 t1)
        (parent-of? t1 t2) (parent-of? t2 t1))]
   [((PrefabTop: key1) (or (PrefabTop: key2)
                           (Prefab:    key2 _)))
    #:no-order
    (or (prefab-key-subtype? key1 key2)
        (prefab-key-subtype? key2 key1))]
   [((Prefab: key1 flds1) (Prefab: key2 flds2))
    (and (or (prefab-key-subtype? key1 key2)
             (prefab-key-subtype? key2 key1))
         (for/and ([fty1 (in-list flds1)]
                   [fty2 (in-list flds2)])
           (overlap? fty1 fty2)))]
   [(_ _) #t]))


;; Type Type -> Boolean
;; Given two struct types, check if the second is a parent struct
;; type of the other (though possibly at different type instantiations
;; if they are polymorphic)
(define (parent-of? t1 t2)
  (match* (t1 t2)
    [((Struct: _ (Struct: pname1 _ _ _ _ _ _) _ _ _ _ _)
      (Struct: pname2 _ _ _ _ _ _))
     #:when (free-identifier=? pname1 pname2)
     #t]
    [((Struct: _ #f _ _ _ _ _)
      other)
     #f]
    [((Struct: _ parent _ _ _ _ _)
      other)
     (parent-of? parent other)]))
