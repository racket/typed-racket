#lang racket/unit

(require "../utils/utils.rkt"
         (rep type-rep type-mask rep-utils)
         (types abbrev subtype resolve overlap)
         "signatures.rkt"
         racket/match
         racket/set)

(import infer^)
(export intersect^)


(define ((intersect-types additive?) t1 t2)
  (cond
    ;; we dispatch w/ Error first, because it behaves in
    ;; strange ways (e.g. it is ⊤ and ⊥ w.r.t subtyping) and
    ;; mucks up what might otherwise be commutative behavior
    [(or (Error? t1) (Error? t2)) Err]
    [else
     (let intersect ([t1 t1] [t2 t2] [seen '()])
       ;; t1   : Type?
       ;; t2   : Type?
       ;; seen : (listof (cons/c (cons/c Type? Type?) symbol?))
       ;; A mapping tracking previously seen instances of potentially
       ;; recursive types in order to prevent infinite looping
       ;; and build a recursive type when appropriate. See the 'resolvable?'
       ;; cases below.
       (define (rec t1 t2) (intersect t1 t2 seen))
       (match* (t1 t2)
         ;; quick overlap check
         [(_ _) #:when (not (overlap? t1 t2)) -Bottom]
      
         ;; already a subtype
         [(_ _) #:when (subtype t1 t2) t1]
         [(_ _) #:when (subtype t2 t1) t2]

     
         ;; polymorphic intersect
         [(t1 (Poly: vars body))
          #:when (infer vars null (list t1) (list body) #f)
          t1]
         [((Poly: vars body) t2)
          #:when (infer vars null (list t2) (list body) #f)
          t2]
     
         ;; structural recursion on types
         [((Pair: a1 d1) (Pair: a2 d2))
          (rebuild -pair (rec a1 a2) (rec d1 d2))]
         ;; FIXME: support structural updating for structs when structs are updated to
         ;; contain not only *if* they are polymorphic, but *which* fields are too
         ;;[((Struct: _ _ _ _ _ _)
         ;; (Struct: _ _ _ _ _ _))]
         [((Syntax: t1*) (Syntax: t2*))
          (rebuild -Syntax (rec t1* t2*))]
         [((Promise: t1*) (Promise: t2*))
          (rebuild -Promise (rec t1* t2*))]

         [((Union: base1 t1s) t2)
          (match t2
            ;; let's be consistent in slamming together unions
            ;; (i.e. if we don't do this dual traversal, the order the
            ;; unions are passed to 'intersect' can produces different
            ;; (albeit equivalent modulo subtyping, we believe) answers)
            [(Union-all: t2s)
             (let ([t1s (if (Bottom? base1) t1s (cons base1 t1s))])
               (apply Un (for*/list ([t1 (in-list t1s)]
                                     [t2 (in-list t2s)]
                                     [t* (in-value (rec t1 t2))]
                                     #:unless (Bottom? t*))
                           t*)))]
            [_ (Union-fmap (λ (t1) (rec t1 t2)) base1 t1s)])]
         [(t1 (Union: base2 t2s)) (Union-fmap (λ (t2) (rec t1 t2)) base2 t2s)]

         [((Intersection: t1s) t2)
          (apply -unsafe-intersect (map (λ (t1) (rec t1 t2)) t1s))]
         [(t1 (Intersection: t2s))
          (apply -unsafe-intersect (map (λ (t2) (rec t1 t2)) t2s))]

         ;; For resolvable types, we record the occurrence and save a back pointer
         ;; in 'seen'. Then, if this pair of types emerges again, we know that we are
         ;; traversing an infinite type tree and instead we insert the back edge
         ;; and create a finite graph (i.e. a μ-type). If the back pointer is never used,
         ;; we don't create a μ-type, we just return the result
         [(t1 t2)
          #:when (or (resolvable? t1) (resolvable? t2))
          (cond
            [(assoc (cons t1 t2) seen)
             ;; we've seen these types before! -- use the stored symbol
             ;; as a back pointer with an 'F' type (i.e. a type variable)
             => (match-lambda
                  [(cons _ record)
                   ;; record that we did indeed use the back
                   ;; pointer by set!-ing the flag
                   (set-mcdr! record #t)
                   (make-F (mcar record))])]
            [else
             ;; we've never seen these types together before! let's gensym a symbol
             ;; so that if we do encounter them again, we can create a μ type.
             (define name (gensym 'rec))
             ;; the 'record' contains the back pointer symbol we may or may not use in
             ;; the car, and a flag for whether or not we actually used the back pointer
             ;; in the cdr.
             (define record (mcons name #f))
             (define t (intersect (resolve t1)
                                  (resolve t2)
                                  (list* (cons (cons t1 t2) record)
                                         (cons (cons t2 t1) record)
                                         seen)))
             (cond
               ;; check if we used the backpointer, if so,
               ;; make a recursive type using that name
               [(mcdr record) (make-Mu name t)]
               ;; otherwise just return the result
               [else t])])]

         ;; Base Unions
         [((BaseUnion: bbits1 nbits1)
           (BaseUnion: bbits2 nbits2))
          (make-BaseUnion (bbits-intersect bbits1 bbits2)
                          (nbits-intersect nbits1 nbits2))]
         [((BaseUnion: bbits nbits)
           (Base-bits: numeric? bits))
          (cond [numeric? (if (nbits-overlap? nbits bits)
                              t2
                              -Bottom)]
                [else (if (bbits-overlap? bbits bits)
                          t2
                          -Bottom)])]
         [((Base-bits: numeric? bits)
           (BaseUnion: bbits nbits))
          (cond [numeric? (if (nbits-overlap? nbits bits)
                              t1
                              -Bottom)]
                [else (if (bbits-overlap? bbits bits)
                          t1
                          -Bottom)])]
         [((BaseUnion-bases: bases1) t2)
          (apply Un (for/list ([b (in-list bases1)])
                      (rec b t2)))]
         [(t1 (BaseUnion-bases: bases2))
          (apply Un (for/list ([b (in-list bases2)])
                      (rec t1 b)))]

         ;; t2 and t1 have a complex relationship, so we build an intersection
         ;; if additive, otherwise t1 remains unchanged
         [(t1 t2) (if additive?
                      (-unsafe-intersect t1 t2)
                      t1)]))]))


;; intersect
;; Type Type -> Type
;;
;; compute the intersection of two types
;; (note: previously called restrict, however now restrict is
;;  a non-additive intersection computation defined below
;; (i.e. only parts of t1 will remain, no parts from t2 are added))
(define intersect (intersect-types #t))

;; restrict
;; Type Type -> Type
;;
;; attempt to compute (t1 - (¬ t2))
;; this is useful when you want to know what part of t1 intersects
;; with t2 without adding t2 to the result (i.e. note that intersect
;; will create an intersection type if the intersection is not obvious,
;; and sometimes we want to make sure and _not_ add t2 to the result
;; we just want to keep the parts of t1 consistent with t2)
(define restrict (intersect-types #f))
