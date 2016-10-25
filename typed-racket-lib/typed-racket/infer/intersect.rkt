#lang racket/unit

(require "../utils/utils.rkt")
(require (rep type-rep type-mask)
         (types abbrev base-abbrev union subtype resolve overlap)
         "signatures.rkt"
         racket/match
         racket/set)

(import infer^)
(export intersect^)

;; compute the intersection of two types
;; (note: previously called restrict, however now restrict is
;;  a non-additive intersection computation defined below
;; (i.e. only parts of t1 will remain, no parts from t2 are added))
(define (intersect t1 t2)
  ;; build-type: build a type while propogating bottom
  (define (build-type constructor . args)
    (if (memf Bottom? args) -Bottom (apply constructor args)))
  (cond
    ;; we dispatch w/ Error first, because it behaves in
    ;; strange ways (e.g. it is ⊤ and ⊥ w.r.t subtyping) and
    ;; mucks up what might otherwise be commutative behavior
    [(or (Error? t1) (Error? t2)) Err]
    [else
     (let intersect
       ([t1 t2] [t2 t1] [seen '()])
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
         [(_ _) #:when (disjoint-masks? (Type-mask t1) (Type-mask t2)) -Bottom]
      
         ;; already a subtype
         [(t1 t2) #:when (subtype t1 t2) t1]
         [(t1 t2) #:when (subtype t2 t1) t2]

     
         ;; polymorphic intersect
         [(t1 (Poly: vars body))
          #:when (infer vars null (list t1) (list body) #f)
          t1]
         [((Poly: vars body) t2)
          #:when (infer vars null (list t2) (list body) #f)
          t2]
     
         ;; structural recursion on types
         [((Pair: a1 d1) (Pair: a2 d2))
          (build-type -pair  (rec a1 a2) (rec d1 d2))]
         ;; FIXME: support structural updating for structs when structs are updated to
         ;; contain not only *if* they are polymorphic, but *which* fields are too
         ;;[((Struct: _ _ _ _ _ _)
         ;; (Struct: _ _ _ _ _ _))]
         [((Syntax: t1*) (Syntax: t2*))
          (build-type -Syntax (rec t1* t2*))]
         [((Promise: t1*) (Promise: t2*))
          (build-type -Promise (rec t1* t2*))]

         [((Union: t1s) t2)
          (match t2
            ;; let's be consistent in slamming together unions
            ;; (i.e. if we don't do this dual traversal, the order the
            ;; unions are passed to 'intersect' can produces different
            ;; albeit equivalent (modulo subtyping, we hope) answers)
            [(Union: t2s) (apply Un (for*/list ([t1 (in-list t1s)]
                                                [t2 (in-list t2s)])
                                      (rec t1 t2)))]
            [_ (apply Un (map (λ (t1) (rec t1 t2)) t1s))])]
         [(t1 (Union: t2s)) (apply Un (map (λ (t2) (rec t1 t2)) t2s))]

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

         ;; t2 and t1 have a complex relationship, so we build an intersection
         ;; (note: intersection checks for overlap)
         [(t1 t2) (-unsafe-intersect t1 t2)]))]))


;; restrict
;; Type Type -> Type
;;
;; attempt to compute (t1 - (¬ t2))
;; this is useful when you want to know what part of t1 intersects
;; with t2 without adding t2 to the result (i.e. note that intersect
;; will create an intersection type if the intersection is not obvious,
;; and sometimes we want to make sure and _not_ add t2 to the result
;; we just want to keep the parts of t1 consistent with t2)
(define (restrict t1 t2)
  ;; build-type: build a type while propogating bottom
  (define (build-type constructor . args)
    (if (memf Bottom? args) -Bottom (apply constructor args)))
  ;; resolved is a set tracking previously seen restrict cases
  ;; (i.e. pairs of t1 t2) to prevent infinite unfolding.
  ;; subtyping performs a similar check for the same
  ;; reason
  (let restrict
    ([t1 t1] [t2 t2] [resolved '()])
    (match* (t1 t2)
      ;; no overlap
      [(_ _) #:when (not (overlap? t1 t2)) -Bottom]
      ;; already a subtype
      [(t1 t2) #:when (subtype t1 t2) t1]
      
      ;; polymorphic restrict
      [(t1 (Poly: vars t)) #:when (infer vars null (list t1) (list t) #f) t1]
      
      ;; structural recursion on types
      [((Pair: a1 d1) (Pair: a2 d2)) 
       (build-type -pair 
                   (restrict a1 a2 resolved) 
                   (restrict d1 d2 resolved))]
      ;; FIXME: support structural updating for structs when structs are updated to
      ;; contain not only *if* they are polymorphic, but *which* fields are too  
      ;;[((Struct: _ _ _ _ _ _)
      ;; (Struct: _ _ _ _ _ _))]
      [((Syntax: t1*) (Syntax: t2*))
       (build-type -Syntax (restrict t1* t2* resolved))]
      [((Promise: t1*) (Promise: t2*))
       (build-type -Promise (restrict t1* t2* resolved))]
      
      ;; unions
      [((Union: t1s) t2)
       (apply Un (map (λ (t1) (restrict t1 t2 resolved)) t1s))]

      [(t1 (Union: t2s))
       (apply Un (map (λ (t2) (restrict t1 t2 resolved)) t2s))]
      
      ;; restrictions
      [((Intersection: t1s) t2)
       (apply -unsafe-intersect (for/list ([t1 (in-list t1s)])
                                  (restrict t1 t2 resolved)))]

      [(t1 (Intersection: t2s))
       (apply -unsafe-intersect (for/list ([t2 (in-list t2s)])
                                  (restrict t1 t2 resolved)))]
      
      ;; resolve resolvable types if we haven't already done so
      [((? resolvable? t1) t2)
       #:when (not (member (cons t1 t2) resolved))
       (restrict (resolve t1) t2 (cons (cons t1 t2) resolved))]

      [(t1 (? resolvable? t2))
       #:when (not (member (cons t1 t2) resolved))
       (restrict t1 (resolve t2) (cons (cons t1 t2) resolved))]
      
      ;; if we're intersecting two recursive types, intersect their body
      ;; and have their recursive references point back to the result
      [((? Mu?) (? Mu?))
       (define name (gensym))
       (make-Mu name (restrict (Mu-body name t1) (Mu-body name t2) resolved))]

      ;; else it's complicated and t1 remains unchanged
      [(_ _) t1])))
