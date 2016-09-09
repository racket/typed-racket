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
;; (note: previously called restrict)
(define (intersect t1 t2)
  ;; build-type: build a type while propogating bottom
  (define (build-type constructor . args)
    (if (memf Bottom? args) -Bottom (apply constructor args)))
  ;; resolved is a set tracking previously seen intersect cases
  ;; (i.e. pairs of t1 t2) to prevent infinite unfolding.
  ;; subtyping performs a similar check for the same
  ;; reason
  (let intersect
      ([t1 t1] [t2 t2] [resolved '()])
    (match*/no-order
     (t1 t2)
     ;; no overlap
     [(_ _) #:when (disjoint-masks? (Type-mask t1) (Type-mask t2))
      -Bottom]
     ;; already a subtype
     [(t1 t2) #:no-order #:when (subtype t1 t2) t1]
     
     ;; polymorphic intersect
     [(t1 (Poly: vars t))
      #:no-order
      #:when (infer vars null (list t1) (list t) #f)
      t1]
     
     ;; structural recursion on types
     [((Pair: a1 d1) (Pair: a2 d2)) 
      (build-type -pair 
                  (intersect a1 a2 resolved) 
                  (intersect d1 d2 resolved))]
     ;; FIXME: support structural updating for structs when structs are updated to
     ;; contain not only *if* they are polymorphic, but *which* fields are too  
     ;;[((Struct: _ _ _ _ _ _)
     ;; (Struct: _ _ _ _ _ _))]
     [((Syntax: t1*) (Syntax: t2*))
      (build-type -Syntax (intersect t1* t2* resolved))]
     [((Promise: t1*) (Promise: t2*))
      (build-type -Promise (intersect t1* t2* resolved))]
     
     ;; unions
     [((Union: t1s) t2)
      #:no-order
      (apply Un (map (λ (t1) (intersect t1 t2 resolved)) t1s))]

     ;; intersections
     [((Intersection: t1s) t2)
      #:no-order
      (apply -unsafe-intersect (for/list ([t1 (in-list t1s)])
                                 (intersect t1 t2 resolved)))]
     
     ;; resolve resolvable types if we haven't already done so
     [((? needs-resolved? t1) t2)
      #:no-order
      #:when (not (member (cons t1 t2) resolved))
      (intersect (resolve t1) t2 (cons (cons t1 t2) resolved))]

     ;; if we're intersecting two recursive types, intersect their body
     ;; and have their recursive references point back to the result
     [((? Mu?) (? Mu?))
      (define name (gensym))
      (make-Mu name (intersect (Mu-body name t1) (Mu-body name t2) resolved))]

     ;; t2 and t1 have a complex relationship, so we build an intersection
     ;; (note: intersection checks for overlap)
     [(t1 t2) (-unsafe-intersect t1 t2)])))


;; attempt to compute (t1 - (¬ t2))
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
      [((? needs-resolved? t1) t2)
       #:when (not (member (cons t1 t2) resolved))
       (restrict (resolve t1) t2 (cons (cons t1 t2) resolved))]

      [(t1 (? needs-resolved? t2))
       #:when (not (member (cons t1 t2) resolved))
       (restrict t1 (resolve t2) (cons (cons t1 t2) resolved))]
      
      ;; if we're intersecting two recursive types, intersect their body
      ;; and have their recursive references point back to the result
      [((? Mu?) (? Mu?))
       (define name (gensym))
       (make-Mu name (restrict (Mu-body name t1) (Mu-body name t2) resolved))]

      ;; else it's complicated and t1 remains unchanged
      [(_ _) t1])))
