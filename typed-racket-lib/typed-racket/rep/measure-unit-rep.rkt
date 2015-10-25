#lang racket/base

(provide make-measure-unit measure-unit:
         make-concrete-measure-unit concrete-measure-unit:
         make-base-measure-unit make-scalar-measure-unit 1-measure-unit make-F-measure-unit
         measure-unit=? u* u^
         )

(require racket/hash
         racket/lazy-require
         racket/list
         racket/match
         "free-variance.rkt"
         "rep-utils.rkt"
         (for-syntax racket/base
                     syntax/parse))
(lazy-require
 [typed-racket/rep/type-rep
  (F? B? Measure? Measure-u)])

;; a base-measure-unit is not a measure-unit
;; name : Symbol
;; id : Symbol
(struct base-measure-unit (name id) #:prefab)

(define (hash-remove-zeros ht)
  (for/hash ([(k v) (in-hash ht)] #:unless (zero? v))
    (values k v)))

(define (non-zero-int? v)
  (and (exact-integer? v)
       (not (eq? v 0))))

;; a potentially polymorphic measure-unit
(def-measure-unit Measure-Unit
  ([s
    (and/c real? positive?)]
   [ht
    (hash/c base-measure-unit? non-zero-int? #:immutable #t)]
   [tbd
    (hash/c (or/c F? B?) non-zero-int? #:immutable #t)])
  [#:frees (λ (f) (combine-frees (map f (hash-keys tbd))))]
  [#:fold-rhs (measure-unit-fold-rhs type-rec-id s ht tbd)]
  )

;; Positive-Real (Hashof base-measure-unit? exact-integer?) (Hashof Type? exact-integer?)
;; -> Measure-Unit
(define (make-measure-unit s ht tbd)
  (*Measure-Unit s
                 (hash-remove-zeros ht)
                 (hash-remove-zeros tbd)))

;; Positive-Real (Hashof base-measure-unit? exact-integer?)
;; -> Measure-Unit
(define (make-concrete-measure-unit s ht)
  (make-measure-unit s ht (hash)))

(define (make-base-measure-unit name id)
  (make-concrete-measure-unit 1 (hash (base-measure-unit name id) 1)))

(define (make-scalar-measure-unit s)
  (make-concrete-measure-unit s (hash)))

(define 1-measure-unit
  (make-scalar-measure-unit 1))

(define (make-F-measure-unit F)
  (make-measure-unit 1 (hash) (hash F 1)))

(define-match-expander measure-unit:
  (syntax-parser
    [(measure-unit: s ht tbd)
     #'(Measure-Unit: s ht tbd)]))

(define-match-expander concrete-measure-unit:
  (syntax-parser
    [(concrete-measure-unit: s ht)
     #'(measure-unit: s ht (hash-table))]))

(define (measure-unit=? a b)
  (or
   (eq? a b)
   (match* [a b]
     [[(measure-unit: s1 ht1 tbd1) (measure-unit: s2 ht2 tbd2)]
      (and (= s1 s2)
           (equal? ht1 ht2)
           (equal? tbd1 tbd2))])))

(define (u* . args)
  (define-values [us ns] (partition MeasureUnit? args))
  (match us
    [(list (measure-unit: ss hts tbds) ...)
     (make-measure-unit (apply * (apply * ns) ss)
                        (apply hash-union (hash) hts #:combine +)
                        (apply hash-union (hash) tbds #:combine +))]))

(define (u^ u n)
  (match u
    [(measure-unit: s ht tbd)
     (make-measure-unit (expt s n)
                        (for/hash ([(k v) (in-hash ht)])
                          (values k (* v n)))
                        (for/hash ([(k v) (in-hash tbd)])
                          (values k (* v n))))]))

(define (measure-unit-fold-rhs type-rec s ht tbd)
  (cond
    [(hash-empty? tbd)
     (make-concrete-measure-unit s ht)]
    [else
     (define tbd+
       (for/list ([(k v) (in-hash tbd)])
         (define t (type-rec k))
         (cond
           [(or (F? t) (B? t))
            (list #f t v)]
           [(Measure? t)
            (list #t (Measure-u t) v)])))
     (define thing
       (map rest (filter first tbd+)))
     (define thing-rst
       (map rest (filter-not first tbd+)))
     (define thing-rst-hsh
       (apply hash (apply append thing-rst)))
     (apply u*
            (make-measure-unit s ht thing-rst-hsh)
            (for/list ([lst (in-list thing)])
              (apply u^ lst)))]))

