#lang racket/unit

(require "../utils/utils.rkt")
(require (rep type-rep)
         (types abbrev base-abbrev union subtype remove-intersect resolve)
         "signatures.rkt"
         racket/match
         racket/set)

(import infer^)
(export restrict^)


;; restrict t1 to be a subtype of t2
;; if `pref' is 'new, use t2 when giving up, otherwise use t1
(define (restrict t1 t2 [pref 'new])
  ;; build-type: build a type while propogating bottom
  (define (build-type constructor . args)
    (if (memf Bottom? args) -Bottom (apply constructor args)))
  ;; resolved is a set tracking previously seen restrict cases
  ;; (i.e. pairs of t1 t2) to prevent infinite unfolding.
  ;; subtyping performs a similar check for the same
  ;; reason
  (define (restrict* t1 t2 pref resolved)
    (match* (t1 t2)
      ;; already a subtype
      [(_ _) #:when (subtype t1 t2) 
             t1]
      
      ;; polymorphic restrict
      [(_ (Poly: vars t)) #:when (infer vars null (list t1) (list t) #f)
                          t1]
      
      ;; structural recursion on types
      [((Pair: a1 d1) (Pair: a2 d2)) 
       (build-type -pair 
                   (restrict* a1 a2 pref resolved) 
                   (restrict* d1 d2 pref resolved))]
      ;; FIXME: support structural updating for structs when structs are updated to
      ;; contain not only *if* they are polymorphic, but *which* fields are too  
      ;;[((Struct: _ _ _ _ _ _)
      ;; (Struct: _ _ _ _ _ _))]
      [((Syntax: t1*) (Syntax: t2*))
       (build-type -Syntax (restrict* t1* t2* pref resolved))]
      [((Promise: t1*) (Promise: t2*))
       (build-type -Promise (restrict* t1* t2* pref resolved))]
      
      ;; unions
      [((Union: t1s) _) (apply Un (map (λ (t1*) (restrict* t1* t2 pref resolved)) t1s))]
      [(_ (Union: t2s)) (apply Un (map (λ (t2*) (restrict* t1 t2* pref resolved)) t2s))]
      
      ;; resolve resolvable types if we haven't already done so
      [((? needs-resolving?) _) #:when (not (set-member? resolved (cons t1 t2)))
                                (restrict* (resolve t1) t2 pref (set-add resolved (cons t1 t2)))]
      [(_ (? needs-resolving?)) #:when (not (set-member? resolved (cons t1 t2)))
                                (restrict* t1 (resolve t2) pref (set-add resolved (cons t1 t2)))]
      
      ;; we don't actually want this - want something that's a part of t1
      [(_ _) #:when (subtype t2 t1)
             t2]
      
      ;; there's no overlap, so the restriction is empty
      [(_ _) #:when (not (overlap t1 t2)) 
             (Un)]
      
      ;; t2 and t1 have a complex relationship, so we punt
      [(_ _) (if (eq? pref 'new) t2 t1)]))
  (restrict* t1 t2 pref (set)))
