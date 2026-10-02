#lang racket/base

;; Functions in this file implement the substitution function in
;; figure 8, pg 8 of "Logical Types for Untyped Languages"

(require "../utils/utils.rkt"
         "../utils/tc-utils.rkt"
         racket/match
         (contract-req)
         "../env/lexical-env.rkt"
         "../types/utils.rkt"
         "../types/prop-ops.rkt"
         "../types/subtype.rkt"
         "../types/path-type.rkt"
         "../types/subtract.rkt"
         "../types/overlap.rkt"
         "../types/variance.rkt"
         (except-in "../types/abbrev.rkt" -> ->* one-of/c)
         (only-in "../infer/infer.rkt" intersect restrict)
         "../rep/core-rep.rkt"
         "../rep/type-rep.rkt"
         "../rep/object-rep.rkt"
         "../rep/prop-rep.rkt"
         "../rep/rep-utils.rkt"
         "../rep/free-variance.rkt"
         "../rep/values-rep.rkt")

(provide instantiate-obj+simplify)

(provide/cond-contract
 [values->tc-results (->* (SomeValues? (listof OptObject?))
                          ((listof Type?))
                          full-tc-results/c)]
 [values->tc-results/explicit-subst
  (-> SomeValues?
      (listof (cons/c exact-nonnegative-integer?
                      (cons/c OptObject?
                              Type?)))
      full-tc-results/c)]
 [erase-identifiers (->* (tc-results/c
                          (listof identifier?))
                         ((listof Type?))
                         tc-results/c)]
 [substitute-identifiers (-> tc-results/c
                             (listof identifier?)
                             (listof OptObject?)
                             (listof Type?)
                             tc-results/c)])


;; Substitutes the given objects into the values and turns it into a
;; tc-result.  This matches up to the substitutions in the T-App rule
;; from the ICFP paper.
;; NOTE! 'os' should contain no unbound relative addresses (i.e. "free" 
;;       De Bruijn indices) as those indices will NOT be updated if they
;;        are substituted under binders.
(define (values->tc-results v objs [types '()])
  (values->tc-results/explicit-subst
   v
   (for/list ([o (in-list objs)]
              [t (in-list/rest types Univ)]
              [idx (in-naturals)])
     (list* idx o t))))

(define (values->tc-results/explicit-subst v subst)
  (define res->tc-res
    (match-lambda
      [(Result: t ps o n-exi) (-tc-result t ps o (not (zero? n-exi)))]))

  (match (instantiate-obj+simplify v subst)
    [(AnyValues: p)
     (-tc-any-results p)]
    [(Values: rs)
     (-tc-results (map res->tc-res rs) #f)]
    [(ValuesDots: rs dty dbound)
     (-tc-results (map res->tc-res rs) (make-RestDots dty dbound))]))

;; erase-identifiers : tc-results (listof identifier) [(listof Type)] -> tc-results
;; Erases the given identifiers, which are going out of scope, from
;; `res`. When the identifiers' types are given, props that those types
;; establish are kept as true rather than erased.
(define (erase-identifiers res names [types (map (λ (_) Univ) names)])
  (substitute-identifiers res names (map (λ (_) -empty-obj) names) types))

;; substitute-identifiers : tc-results (listof identifier) (listof OptObject)
;;                          (listof Type) -> tc-results
;; Replaces each of `names` in `res` with the corresponding object, which
;; denotes a value of the corresponding type.
(define (substitute-identifiers res names objs types)
  (define entries (map cons objs types))
  (subst-objs+simplify
   res
   (λ (nm _)
     (and (identifier? nm)
          (for/first ([name (in-list names)]
                      [entry (in-list entries)]
                      #:when (free-identifier=? nm name))
            entry)))))

;; instantiate-obj+simplify : Rep (listof (list* idx OptObject Type)) -> Rep
;; Replaces each De Bruijn index (0 . idx), the idx-th argument of the
;; function whose range is `rep`, with the corresponding object.
(define (instantiate-obj+simplify rep mapping)
  (subst-objs+simplify
   rep
   (λ (nm lvl)
     (match nm
       [(cons (== lvl) idx)
        (match (assv idx mapping)
          [(cons _ entry) entry]
          [_ #f])]
       [_ #f]))))

;; A Polarity describes how a prop at some position is used, and so in
;; which direction substitution may change it:
;;  #t -- positive: the prop is a fact the typechecker learns, so it may
;;        soundly be weakened
;;  #f -- negative (underneath a function domain, or a negated type): the
;;        prop is an obligation the context must discharge, so it may
;;        only be strengthened
;;  (-> none/c) -- invariant (underneath a mutable container): the prop
;;        may be neither weakened nor strengthened. Calling the procedure
;;        gives up on the nearest enclosing type whose polarity is known,
;;        replacing that type with its top type in a positive position
;;        or with Bottom in a negative one.
;;
;; When a variable is substituted away by the empty object, props
;; mentioning it can no longer be expressed: they become tt in positive
;; positions but must become ff in negative ones, since erasing an
;; obligation to tt would let arguments that never discharge it slip
;; through. This follows the polarity-indexed substitution of figure 8
;; of the paper, and object substitution in "Rebuild λTR", where the
;; polarity is carried by the object (⊤ or ⊥) that replaces the variable
;; and flips at function domains and underneath ∉. λTR has no mutable
;; containers; widening an invariant position to its top type (or Bottom)
;; preserves its subtyping-for-erasure lemma, A⁻ <: A <: A⁺.
(define (flip pol) (if (procedure? pol) pol (not pol)))

;; the prop that replaces one whose subject is erased
(define (erased-prop pol)
  (cond [(procedure? pol) (pol)]
        [pol -tt]
        [else -ff]))

;; the props of a result whose object is erased: in a negative position
;; the object is an obligation that can no longer be stated, so no value
;; may satisfy the result (λTR uses the bottom object here)
(define (erased-result-props ps pol)
  (cond [(procedure? pol) (pol)]
        [pol ps]
        [else -ff-propset]))

;; with-widening : Polarity Type ((-> none/c) -> Type) -> Type
;; Calls `k` with the polarity to use for the invariant parts of a type
;; at polarity `pol`; if those parts mention an erased object, the whole
;; type becomes `top` (or Bottom, in a negative position).
(define (with-widening pol top k)
  (if (procedure? pol)
      (k pol)
      (let/ec esc
        (k (λ () (esc (if pol top -Bottom)))))))

;; subst-objs+simplify : Rep (name-ref/c natural -> (or/c #f (cons OptObject Type))) -> Rep
;; Replaces each object whose name `lookup` maps, given the number of
;; binders the object is underneath, with the object `lookup` returns,
;; which denotes a value of the type `lookup` returns.
(define (subst-objs+simplify rep lookup)
  (let subst/lvl ([rep rep] [lvl 0] [pol #t])
    (define (subst rep) (subst/lvl rep lvl pol))
    (define (subst/flip rep) (subst/lvl rep lvl (flip pol)))
    (define (subst/pol rep pol) (subst/lvl rep lvl pol))
    (define (lookup* nm) (lookup nm lvl))
    ;; substitutes a result's object; also returns the type that the
    ;; substituted object is known to have
    (define (subst-result-obj o)
      (match o
        [(Path: flds (app lookup* (? pair? entry)))
         (values (make-Path (map subst flds) (car entry))
                 (or (path-type flds (cdr entry)) Univ))]
        [_ (values (and o (subst o)) #f)]))
    ;; substitutes a result's type, props, and object
    (define (subst-result orig-t orig-ps orig-o)
      (define-values (o o-ty) (subst-result-obj orig-o))
      (define t (if (and o-ty (not (Univ? o-ty)))
                    (intersect (subst orig-t) o-ty)
                    (subst orig-t)))
      (define ps (cond
                   [(not orig-ps) #f]
                   [(and (Empty? o) (Object? orig-o))
                    (erased-result-props (subst orig-ps) pol)]
                   [else (subst orig-ps)]))
      (values t ps o))
    (match rep
      ;; Functions
      ;; increment the level of the substituted object;
      ;; the domain (incl. rest/keyword args) is a negative position
      [(Arrow: dom rst kws rng rng-T+)
       (make-Arrow (map subst/flip dom)
                   (and rst (subst/flip rst))
                   (map subst/flip kws)
                   (subst/lvl rng (add1 lvl) pol)
                   rng-T+)]
      [(DepFun: dom pre rng)
       (make-DepFun (for/list ([d (in-list dom)])
                      (subst/lvl d (add1 lvl) (flip pol)))
                    (subst/lvl pre (add1 lvl) (flip pol))
                    (subst/lvl rng (add1 lvl) pol))]
      [(Intersection: ts raw-prop)
       (-refine (make-Intersection (map subst ts))
                (subst/lvl raw-prop (add1 lvl) pol))]
      [(Path: flds (app lookup* (cons o _)))
       (make-Path (map subst flds) o)]
      ;; restrict with the type for results and props
      [(TypeProp: (Path: flds (app lookup* (? pair? entry))) raw-prop-ty)
       (define o (make-Path (map subst flds) (car entry)))
       (define o-ty (or (path-type flds (cdr entry)) Univ))
       (define prop-ty (subst raw-prop-ty))
       (define new-prop-ty (intersect prop-ty o-ty o))
       (cond
         [(Bottom? new-prop-ty) -ff]
         [(and (not (F? prop-ty))  (subtype o-ty prop-ty)) -tt]
         [(Empty? o) (erased-prop pol)]
         [else (-is-type o new-prop-ty)])]
      [(NotTypeProp: (Path: flds (app lookup* (? pair? entry))) raw-prop-ty)
       (define o (make-Path (map subst flds) (car entry)))
       (define o-ty (or (path-type flds (cdr entry)) Univ))
       ;; the type in a NotTypeProp is underneath a negation,
       ;; so polarity flips
       (define prop-ty (subst/flip raw-prop-ty))
       (define new-o-ty (subtract o-ty prop-ty o))
       (define new-prop-ty (restrict prop-ty o-ty o))
       (cond
         [(or (Bottom? new-o-ty)
              (Univ? new-prop-ty))
          -ff]
         ;; no overlap between the type of the object and the
         ;; negated type: the prop is known to hold
         [(Bottom? new-prop-ty) -tt]
         [(Empty? o) (erased-prop pol)]
         [else (-not-type o new-prop-ty)])]
      ;; other subjects, such as linear expressions, may become empty too
      [(TypeProp: obj prop-ty)
       (define new-obj (subst obj))
       (if (Empty? new-obj)
           (erased-prop pol)
           (-is-type new-obj (subst prop-ty)))]
      [(NotTypeProp: obj prop-ty)
       (define new-obj (subst obj))
       (if (Empty? new-obj)
           (erased-prop pol)
           (-not-type new-obj (subst/flip prop-ty)))]
      [(LeqProp: lhs rhs)
       (define new-lhs (subst lhs))
       (define new-rhs (subst rhs))
       (if (or (Empty? new-lhs) (Empty? new-rhs))
           (erased-prop pol)
           (make-LeqProp new-lhs new-rhs))]
      [(tc-result: orig-t orig-ps orig-o exi?)
       (define-values (t ps o) (subst-result orig-t orig-ps orig-o))
       (-tc-result t ps o exi?)]
      [(Result: orig-t orig-ps orig-o n-exi)
       (define-values (t ps o) (subst-result orig-t orig-ps orig-o))
       (make-Result t ps o n-exi)]
      ;; types with contravariant or invariant parameters
      [(app Rep-variances (? pair? variances))
       (define (subst-args inv)
         (apply (Rep-constructor rep)
                (for/list ([t (in-list (Rep-values rep))]
                           [v (in-list variances)])
                  (cond
                    [(variance:co? v) (subst t)]
                    [(variance:contra? v) (subst/flip t)]
                    [else (subst/pol t inv)]))))
       (if (memq variance:inv variances)
           (with-widening pol (top-of rep) subst-args)
           (subst-args pol))]
      [(App: rator rands)
       (define variances
         (or (app-variances rator rands)
             (map (λ (_) variance:inv) rands)))
       (define (subst-args inv)
         (make-App rator
                   (for/list ([t (in-list rands)]
                              [v (in-list variances)])
                     (cond
                       [(or (variance:co? v) (variance:const? v)) (subst t)]
                       [(variance:contra? v) (subst/flip t)]
                       [else (subst/pol t inv)]))))
       (if (ormap (λ (v) (or (variance:inv? v) (variance:dotted? v))) variances)
           (with-widening pol Univ subst-args)
           (subst-args pol))]
      [(? invariant-type?)
       (with-widening pol (top-of rep)
         (λ (inv) (Rep-fmap rep (λ (r) (subst/pol r inv)))))]
      ;; else default fold over subfields
      [_ (Rep-fmap rep subst)])))


