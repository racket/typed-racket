#lang racket/unit

(require "../utils/utils.rkt"
	 (types abbrev subtype)
	 racket/dict
     "fail.rkt" "signatures.rkt" "constraint-structs.rkt"
     racket/match
     racket/set
     racket/stream)

(import intersect^ dmap^)
(export constraints^)

;; Widest constraint possible
(define no-constraint (make-c (Un) Univ))

;; Create an empty constraint map from a set of type variables X and
;; index variables Y.  For now, we add the widest constraints for
;; variables in X to the cmap and create an empty dmap.
(define (empty-cset X Y)
  (make-cset (list (cons (for/hash ([x (in-list X)])
                           (values x no-constraint))
                         (make-dmap (make-immutable-hash null))))))


;; add the constraints S <: var <: T to every map in cs
(define (insert cs var S T)
  (match cs
    [(struct cset (maps))
     (make-cset (for/list ([map-entry (in-list maps)])
                  (match-define (cons map dmap) map-entry)
                  (cons (hash-set map var (make-c S T))
                        dmap)))]))

;; meet: Type Type -> Type
;; intersect the given types, producing the greatest lower bound
(define (meet S T) (intersect S T))

;; join: Type Type -> Type
;; union the given types, producing the least upper bound
(define (join T U) (Un T U))


;; meet of two variable constraints
;; never fails
;; if var is provided, the resulting constraint uses it, otherwise it
;; uses the variable from `c1` (which must be the same as the one from
;; `c2`)
(define (c-meet c1 c2 [var #f])
  (match*/early (c1 c2)
    [((struct c (S T)) (struct c (S* T*)))
     (let ([S (join S S*)] [T (meet T T*)])
       (and (subtype S T)
            (make-c S T)))]))

;; compute the meet of two constraint sets
;; returns #f for failure
(define cset-meet
  (case-lambda
    [() (empty-cset null null)]
    [(x) x]
    [(x y)
     (match* (x y)
      [((struct cset (maps1)) (struct cset (maps2)))
       (define maps
         (for*/stream ([md1 (in-stream (stream-remove-duplicates maps1))]
                       [md2 (in-stream (stream-remove-duplicates maps2))]
                       [v (in-value
                            (% cons
                               (hash-union/fail (car md1) (car md2) #:combine c-meet)
                               (dmap-meet (cdr md1) (cdr md2))))]
                       #:when v)
           v))
       (cond [(stream-empty? maps)
              #f]
             [else (make-cset maps)])])]
    [(x . ys)
     (for/fold ([x x]) ([y (in-list ys)])
       (% cset-meet x y))]))

;; combines a list of csets using cset-meet individually
;; returns #f for failure
(define (cset-meet* args)
  (apply cset-meet args))

;; produces a cset of all of the maps in all of the given csets
;; FIXME: should this call `remove-duplicates`?
(define (cset-join l)
  (let ([mapss (map cset-maps l)])
    (make-cset (apply stream-append mapss))))

(define (stream-remove-duplicates st)
  (define seen (mutable-set))
  (define (new-elem? elem)
    (and (not (set-member? seen elem))
         (set-add! seen elem)
         #true))
  (stream-filter new-elem? st))
