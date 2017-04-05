#lang racket/base

(require "../utils/utils.rkt"
         (contract-req)
         "fme-utils.rkt"
         "rep-utils.rkt"
         "free-variance.rkt"
         "core-rep.rkt"
         "object-rep.rkt"
         racket/match
         racket/lazy-require
         (only-in racket/unsafe/ops unsafe-fx<=))

(lazy-require
 ["../types/prop-ops.rkt" (-and -or negate-prop)])

(provide -is-type
         -not-type)

(provide/cond-contract
 [-leq (-> OptObject? OptObject? Prop?)]
 [-lt (-> OptObject? OptObject? Prop?)]
 [-gt (-> OptObject? OptObject? Prop?)]
 [-geq (-> OptObject? OptObject? Prop?)]
 [-eq (-> OptObject? OptObject? Prop?)])


(def-prop TypeProp ([obj Object?] [type (and/c Type? (not/c Univ?) (not/c Bottom?))])
  [#:frees (f) (combine-frees (list (f obj) (f type)))]
  [#:fmap (f) (make-TypeProp (f obj) (f type))]
  [#:for-each (f) (begin (f obj) (f type))]
  [#:custom-constructor/contract
   (-> OptObject? Type? Prop?)
   (match* (obj type)
     [((Empty:) _) -tt]
     [(_ (Univ:)) -tt]
     [(_ (Bottom:)) -ff]
     [(_ _) (intern-double-ref!
             tprop-intern-table
             obj type #:construct (make-TypeProp obj type))])])

(define tprop-intern-table (make-weak-hash))

;; Abbreviation for props
;; `i` can be an integer or name-ref/c for backwards compatibility
;; FIXME: Make all callers pass in an object and remove backwards compatibility
(define/cond-contract (-is-type i t)
  (-> (or/c integer? name-ref/c OptObject?) Type? Prop?)
  (define o
    (cond
      [(OptObject? i) i]
      [(exact-integer? i) (make-Path null (cons 0 i))]
      [(pair? i) (make-Path null i)]
      [else (-id-path i)]))
  (make-TypeProp o t))

(def-prop NotTypeProp ([obj Object?] [type (and/c Type? (not/c Univ?) (not/c Bottom?))])
  [#:frees (f) (combine-frees (list (f obj) (f type)))]
  [#:fmap (f) (-not-type (f obj) (f type))]
  [#:for-each (f) (begin (f obj) (f type))]
  [#:custom-constructor/contract
   (-> OptObject? Type? Prop?)
   (match* (obj type)
     [((Empty:) _) -tt]
     [(_ (Univ:)) -ff]
     [(_ (Bottom:)) -tt]
     [(_ _) (intern-double-ref!
             ntprop-intern-table
             obj type #:construct (make-NotTypeProp obj type))])])

(define ntprop-intern-table (make-weak-hash))

;; Abbreviation for not props
;; `i` can be an integer or name-ref/c for backwards compatibility
;; FIXME: Make all callers pass in an object and remove backwards compatibility
(define/cond-contract (-not-type i t)
  (-> (or/c integer? name-ref/c OptObject?) Type? Prop?)
  (define o
    (cond
      [(OptObject? i) i]
      [(exact-integer? i) (make-Path null (cons 0 i))]
      [(pair? i) (make-Path null i)]
      [else (-id-path i)]))
  (make-NotTypeProp o t))


;; an inequality between two linear inequalities
(def-prop LeqProp ([lhs LExp?] [rhs LExp?])
  [#:frees (f) (combine-frees (list (f lhs) (f rhs)))]
  [#:fmap (f) (make-LeqProp (f lhs) (f rhs))]
  [#:for-each (f) (f lhs) (f rhs)]
  ;; a custom constructor which reduces trivially valid/invalid
  [#:custom-constructor/contract
   (-> OptObject? OptObject? (or/c LeqProp? TrueProp?))
   (let-values
       ([(lhs rhs)
         (match lhs
           [(? LExp?)
            (match rhs
              [(? LExp?) (values lhs rhs)]
              [(? Path?) (values lhs (raw-make-LExp 0 (make-terms rhs 1)))]
              [(? Empty?) (values #f #f)])]
           [(? Path?)
            (let ([lhs (raw-make-LExp 0 (make-terms lhs 1))])
              (match rhs
                [(? LExp?) (values lhs rhs)]
                [(? Path?) (values lhs (raw-make-LExp 0 (make-terms rhs 1)))]
                [(? Empty?) (values #f #f)]))]
           [(? Empty?) (values #f #f)])])
     (match* (lhs rhs)
       [((? LExp?) (? LExp?))
        (cond
          ;; if the inequality is trivially true or false,
          ;; return the appropriate base prop (tt or ff)
          [(eq? lhs rhs) -tt]
          [(constant-LExp? lhs)
           => (λ (l-const)
                (cond
                  [(constant-LExp? rhs)
                   => (λ (r-const) (if (<= l-const r-const) -tt -ff))]
                  [else
                   (intern-double-ref!
                    Leq-intern-table
                    lhs rhs #:construct (make-LeqProp lhs rhs))]))]
          [else
           (intern-double-ref!
            Leq-intern-table
            lhs rhs #:construct (make-LeqProp lhs rhs))])]
       [(#f _) -tt]
       [(_ #f) -tt]))])

(define Leq-intern-table (make-weak-hash))

(define -leq make-LeqProp)
(define (-lt lhs rhs) (make-LeqProp (-lexp-add1 lhs) rhs))
(define (-gt lhs rhs) (-lt rhs lhs))
(define (-geq lhs rhs) (-leq rhs lhs))
(define (-eq l1 l2) (-and (-leq l1 l2) (-leq l2 l1)))

(def-prop OrProp ([ps (listof (or/c TypeProp? NotTypeProp? LeqProp?))])
  [#:frees (f) (combine-frees (map f ps))]
  [#:fmap (f) (apply -or (map f ps))]
  [#:for-each (f) (for-each f ps)]
  [#:custom-constructor/contract
   (-> (listof (or/c TypeProp? NotTypeProp? LeqProp?)) OrProp?)
   (let ([ps (sort ps (λ (p q) (unsafe-fx<= (eq-hash-code p)
                                            (eq-hash-code q))))])
     (intern-single-ref!
      orprop-intern-table
      ps
      #:construct (make-OrProp ps)))])

(define orprop-intern-table (make-weak-hash))

(def-prop AndProp ([ps (listof (or/c OrProp? TypeProp? NotTypeProp? LeqProp?))])
  [#:frees (f) (combine-frees (map f ps))]
  [#:fmap (f) (apply -and (map f ps))]
  [#:for-each (f) (for-each f ps)])
