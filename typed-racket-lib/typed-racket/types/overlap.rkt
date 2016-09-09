#lang racket/base

(require "../utils/utils.rkt"
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
  (let ([a (cons t1 t2)]
        [b (cons t2 t1)])
    (for/or ([p (in-list (current-seen))])
      (or (equal? p a)
          (equal? p b)))))
(define-syntax-rule (with-updated-seen t1 t2 body ...)
  (parameterize ([current-seen (cons (cons t1 t2) (current-seen))])
    body ...))

;; overlap?
;; Type Type -> Boolean
;; a conservative check to see if two types
;; have a non-empty intersection
(define/cond-contract (overlap? t1 t2)
  (c:-> Type? Type? boolean?)
  (cond
    [(type-equal? t1 t2) #t]
    [(disjoint-masks? (Type-mask t1) (Type-mask t2)) #f]
    [(seen? t1 t2) #t]
    [else
     (with-updated-seen
       t1 t2
       (match*/no-order
        (t1 t2)
        [((Univ:) _) #:no-order #t]
        [((or (B: _) (F: _)) _) #:no-order #t]
        [((Opaque: _) _) #:no-order #t]
        [((Name/simple: n) (Name/simple: n*))
         (or (free-identifier=? n n*)
             (overlap? (resolve-once t1) (resolve-once t2)))]
        [(t (or (? Name? s)
                (? App? s)))
         #:no-order
         (overlap? t (resolve-once s))]
        [((? Mu? t) s) #:no-order (overlap? (unfold t) s)]
        [((Refinement: t _) s) #:no-order (overlap? t s)]
        [((Union: ts) s)
         #:no-order
         (ormap (λ (t) (overlap? t s)) ts)]
        [((Intersection: ts) s)
         #:no-order
         (for/and ([t (in-list ts)])
           (overlap? t s))]
        [((? Poly?) _) #:no-order #t] ;; conservative
        [((Base: s1 _ _ _) (Base: s2 _ _ _)) (or (subtype t1 t2) (subtype t2 t1))]
        [((? Base? t) (? Value? s)) #:no-order (subtype s t)] ;; conservative
        [((Syntax: t) (Syntax: t*)) (overlap? t t*)]
        [((Syntax: _) _) #:no-order #f]
        [((Base: _ _ _ _) _) #:no-order #f]
        [((Value: (? pair?)) (Pair: _ _)) #:no-order #t]
        [((Pair: a b) (Pair: a* b*)) (and (overlap? a a*)
                                          (overlap? b b*))]
        ;; lots of things are sequences, but not values where sequence? produces #f
        [((Sequence: _) (Value: v)) #:no-order (sequence? v)]
        ;; hash tables are two-valued sequences
        [((Sequence: (or (list _) (list _ _ _ ...)))
          (or (? Hashtable?) (? HashtableTop?)))
         #:no-order
         #f]
        ;; these are single-valued sequences
        [((Sequence: (list _ _ _ ...))
          (or (? Pair?) (? Vector?) (? VectorTop?)))
         #:no-order
         #f]
        ;; be conservative about other kinds of sequences
        [((Sequence: _) _) #:no-order #t]
        ;; Values where evt? produces #f cannot be Evt
        [((Evt: _) (Value: v)) #:no-order (evt? v)]
        [((Pair: _ _) _) #:no-order #f]
        [((Value: (? simple-datum? v1))
          (Value: (? simple-datum? v2)))
         (equal? v1 v2)]
        [((Value: (? simple-datum?))
          (or (? Struct?) (? StructTop?) (? Function?)))
         #:no-order
         #f]
        [((Value: (not (? hash?)))
          (or (? Hashtable?) (? HashtableTop?)))
         #:no-order
         #f]
        [((Struct: n _ flds _ _ _)
          (Struct: n* _ flds* _ _ _))
         #:when (free-identifier=? n n*)
         (for/and ([f (in-list flds)] [f* (in-list flds*)])
           (match* (f f*)
             [((fld: t _ _) (fld: t* _ _)) (overlap? t t*)]))]
        [((Struct: n #f _ _ _ _)
          (StructTop: (Struct: n* #f _ _ _ _)))
         #:when (free-identifier=? n n*)
         #t]
        ;; n and n* must be different, so there's no overlap
        [((Struct: n #f flds _ _ _)
          (Struct: n* #f flds* _ _ _))
         #f]
        [((Struct: n #f flds _ _ _)
          (StructTop: (Struct: n* #f flds* _ _ _)))
         #f]
        [((and t1 (Struct: _ _ _ _ _ _))
          (and t2 (Struct: _ _ _ _ _ _)))
         (or (subtype t1 t2) (subtype t2 t1)
             (parent-of? t1 t2) (parent-of? t2 t1))]
        [(_ _) #t]))]))


;; Type Type -> Boolean
;; Given two struct types, check if the second is a parent struct
;; type of the other (though possibly at different type instantiations
;; if they are polymorphic)
(define (parent-of? t1 t2)
  (match* (t1 t2)
    [((Struct: _ (Struct: pname1 _ _ _ _ _) _ _ _ _)
      (Struct: pname2 _ _ _ _ _))
     #:when (free-identifier=? pname1 pname2)
     #t]
    [((Struct: _ #f _ _ _ _)
      other)
     #f]
    [((Struct: _ parent _ _ _ _)
      other)
     (parent-of? parent other)]))
