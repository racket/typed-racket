#lang racket/unit

(require "../utils/utils.rkt"
         (utils prefab)
         (rep type-rep object-rep type-mask rep-utils)
         (types abbrev subtype resolve overlap)
         "signatures.rkt"
         racket/match
         racket/set)

(import infer^)
(export intersect^)

;; intersect-types
;;
;; this defines both intersect and restrict, depending on
;; whether or not additive? is #f. If additive is #f, then
;; we're strictly 'restricting' the type of t1 to only contain
;; the parts that are compatible with t2. If additive is #t,
;; then we are intersecting with the intent of saying it is
;; of type t1 and type t2, and so we may get a "larger"
;; looking type out as a result (i.e. a literal intersection
;; (∩ ...) if we can't figure out exactly how the types relate)
(define ((intersect-types additive?) t1 t2 seen obj)
  (let intersect ([t1 t1] [t2 t2] [seen seen] [obj obj])
    ;; t1   : Type?
    ;; t2   : Type?
    ;; seen : (listof (cons/c (cons/c Type? Type?) symbol?))
    ;; A mapping tracking previously seen instances of potentially
    ;; recursive types in order to prevent infinite looping
    ;; and build a recursive type when appropriate. See the 'resolvable?'
    ;; cases below.
    (define (rec t1 t2 [obj -empty-obj]) (intersect t1 t2 seen obj))
    (match* (t1 t2)
      ;; quick overlap check
      [(_ _) #:when (not (overlap? t1 t2)) -Bottom]
      
      ;; already a subtype
      [(_ _) #:when (subtype t1 t2 obj) t1]
      [(_ _) #:when (subtype t2 t1 obj) t2]

     
      ;; polymorphic intersect
      [(t1 (Poly: vars body))
       #:when (infer vars null (list t1) (list body) #f)
       t1]
      [((Poly: vars body) t2)
       #:when (infer vars null (list t2) (list body) #f)
       t2]
     
      ;; structural recursion on types
      [((Pair: a1 d1) (Pair: a2 d2))
       (rebuild -pair (rec a1 a2 (-car-of obj)) (rec d1 d2 (-cdr-of obj)))]
      ;; FIXME: support structural updating for structs when structs are updated to
      ;; contain not only *if* they are polymorphic, but *which* fields are too
      ;;[((Struct: _ _ _ _ _ _)
      ;; (Struct: _ _ _ _ _ _))]
     [((Prefab: key1 flds1) (Prefab: key2 flds2))
       #:when (and (or (prefab-key-subtype? key1 key2)
                       (prefab-key-subtype? key2 key1))
                   (not (prefab-key/mutable-fields? key1))
                   (not (prefab-key/mutable-fields? key2)))
       (define-values (resulting-key extra-fields)
         (if (prefab-key-subtype? key1 key2)
             (values key1 (list-tail flds1 (length flds2)))
             (values key2 (list-tail flds2 (length flds1)))))
       (define flds* (for/list ([fty1 (in-list flds1)]
                                [fty2 (in-list flds2)]
                                [n (in-naturals)])
                       (rec fty1 fty2 (-prefab-idx-of resulting-key n obj))))
       (cond
         [(ormap Bottom? flds*) -Bottom]
         [else (make-Prefab resulting-key
                            (append flds* extra-fields))])]
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
                                  [t* (in-value (rec t1 t2 obj))]
                                  #:unless (Bottom? t*))
                        t*)))]
         [_ (Union-fmap (λ (t1) (rec t1 t2 obj)) base1 t1s)])]
      [(t1 (Union: base2 t2s)) (Union-fmap (λ (t2) (rec t1 t2 obj)) base2 t2s)]

      [((Intersection: t1s raw-prop) t2)
       (-refine (apply -unsafe-intersect (map (λ (t1) (rec t1 t2 obj)) t1s))
                raw-prop)]
      [(t1 (Intersection: t2s raw-prop))
       (-refine (apply -unsafe-intersect (map (λ (t2) (rec t1 t2 obj)) t2s))
                raw-prop)]

      [(t1 t2) #:when (or (resolvable? t1) (resolvable? t2))
               (resolvable-intersect t1 t2 seen obj additive?)]

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
                   (rec b t2 obj)))]
      [(t1 (BaseUnion-bases: bases2))
       (apply Un (for/list ([b (in-list bases2)])
                   (rec t1 b obj)))]

      ;; t2 and t1 have a complex relationship, so we build an intersection
      ;; if additive, otherwise t1 remains unchanged
      [(t1 t2) (if additive?
                   (-unsafe-intersect t1 t2)
                   t1)])))



;; resolvable-intersect
;;
;; helper function for intersecting resolvable types.
;; 
;; For resolvable types we want to be careful for a few reasons:
;; 1. if it's a resolvable type that's not yet fully defined,
;;    we want to short-circuit and just build the intersection
;; 2. when intersecting two infinite types we want an accurate answer
;;    (i.e. try to see where the infinite trees overlap and how to
;;    merge them intelligently), so we record the occurrence and save
;;    a back pointer in 'seen'. Then, if this pair of types emerges
;;    again, we know that we are traversing an infinite type tree and
;;    instead we insert the back edge and create a finite graph (i.e. a μ-type).
;;    If the back pointer is never used, we don't create a μ-type, we just
;;    return the result
(define (resolvable-intersect initial-t1 initial-t2 seen obj additive?)
  (let ([t1 (if (resolvable? initial-t1)
                (resolve-once initial-t1)
                initial-t1)])
    (cond
      [(assoc (cons initial-t1 initial-t2) seen)
       ;; we've seen these types before! -- use the stored symbol
       ;; as a back pointer with an 'F' type (i.e. a type variable)
       => (match-lambda
            [(cons _ record)
             ;; record that we did indeed use the back
             ;; pointer by set!-ing the flag
             (set-mcdr! record #t)
             (make-F (mcar record))])]
      ;; if t1 is not a fully defined type, do the simple thing
      [(not t1) (if additive?
                    (-unsafe-intersect initial-t1 initial-t2)
                    initial-t1)]
      [else
       (let ([t2 (if (resolvable? initial-t2)
                     (resolve-once initial-t2)
                     initial-t2)])
         (cond
           ;; if t2 is not a fully defined type, do the simple thing
           [(not t2) (if additive?
                         (-unsafe-intersect t1 initial-t2)
                         t1)]
           [else
            ;; we've never seen these types together before! let's gensym a symbol
            ;; so that if we do encounter them again, we can create a μ type.
            (define name (gensym 'rec))
            ;; the 'record' contains the back pointer symbol we may or may not use in
            ;; the car, and a flag for whether or not we actually used the back pointer
            ;; in the cdr.
            (define record (mcons name #f))
            (define seen* (list* (cons (cons initial-t1 initial-t2) record)
                                 (cons (cons initial-t2 initial-t1) record)
                                 seen))
            (define t (cond
                        [additive? (internal-intersect t1 t2 seen* obj)]
                        [else (internal-restrict t1 t2 seen* obj)]))
            (cond
              ;; check if we used the backpointer, if so,
              ;; make a recursive type using that name
              [(mcdr record) (make-Mu name t)]
              ;; otherwise just return the result
              [else t])]))])))


;; intersect
;; Type Type -> Type
;;
;; compute the intersection of two types
;; (note: previously called restrict, however now restrict is
;;  a non-additive intersection computation defined below
;; (i.e. only parts of t1 will remain, no parts from t2 are added))
(define (intersect t1 t2 [obj -empty-obj])
  (cond
    ;; since Err is ⊤ and ⊥, check first to ensure
    ;; more predictable behavior
    [(or (Error? t1) (Error? t2)) Err]
    [else (internal-intersect t1 t2 '() obj)]))

(define internal-intersect (intersect-types #t))

;; restrict
;; Type Type -> Type
;;
;; attempt to compute (t1 - (¬ t2))
;; this is useful when you want to know what part of t1 intersects
;; with t2 without adding t2 to the result (i.e. note that intersect
;; will create an intersection type if the intersection is not obvious,
;; and sometimes we want to make sure and _not_ add t2 to the result
;; we just want to keep the parts of t1 consistent with t2)
(define (restrict t1 t2 [obj -empty-obj])
  (cond
    ;; since Err is ⊤ and ⊥, check first to ensure
    ;; more predictable behavior
    [(or (Error? t1) (Error? t2)) Err]
    [else (internal-restrict t1 t2 '() obj)]))

(define internal-restrict (intersect-types #f))
