#lang racket/base

(require "../utils/utils.rkt"
         "../rep/type-rep.rkt"
         "../rep/values-rep.rkt"
         "../rep/prop-rep.rkt"
         "../rep/rep-utils.rkt"
         "../rep/free-variance.rkt"
         "../types/abbrev.rkt"
         "../types/utils.rkt"
         "../types/variance.rkt"
         (prefix-in c: (contract-req))
         racket/list racket/match)
(provide/cond-contract
  [var-promote (c:-> Type? (c:listof symbol?) Type?)]
  [var-demote (c:-> Type? (c:listof symbol?) Type?)])

(define (V-in? V . ts)
  (for/or ([e (in-list (append-map fv ts))])
    (memq e V)))

;; var-promote : Type (Listof Symbol) -> Type
;; var-demote : Type (Listof Symbol) -> Type
;; The least supertype (greatest subtype) of T that does not mention the
;; type variables V, as in Pierce and Turner's "Local Type Inference".
;; Variables are replaced by Univ or Bottom according to the variance of
;; their position. A type that mentions V in an invariant position has no
;; such supertype (subtype) of the same shape, so it is replaced as a
;; whole by its top type (Bottom); replacing just the variable, say from
;; (Boxof X) to (Boxof Any), would give an unrelated type.
(define (var-promote T V)
  (var-change V T #t))
(define (var-demote T V)
  (var-change V T #f))

(define (var-change V cur change)
  (define (co t) (var-change V t change))
  (define (contra t) (var-change V t (not change)))
  (define (mentions-V? t) (V-in? V t))
  ;; the replacement for all of `cur`, which mentions V invariantly
  (define (give-up) (if change (top-of cur) -Bottom))
  ;; changes the parts of `cur` with the given variances
  (define (change-parts mk parts variances)
    (if (for/or ([t (in-list parts)]
                 [v (in-list variances)])
          (and (not (or (variance:co? v) (variance:contra? v) (variance:const? v)))
               (mentions-V? t)))
        (give-up)
        (apply mk (for/list ([t (in-list parts)]
                             [v (in-list variances)])
                    (cond
                      [(variance:contra? v) (contra t)]
                      [(variance:co? v) (co t)]
                      [else t])))))
  (match cur
    [(F: name) (if (memq name V)
                   (if change Univ -Bottom)
                   cur)]
    ;; the domain is contravariant; propositions in the range that
    ;; mention V are weakened (when promoting) or strengthened (when
    ;; demoting) like any other type in the range
    [(Arrow: dom rst kws rng rng-T+)
     (make-Arrow (map contra dom)
                 (if (and (RestDots? rst) (memq (RestDots-nm rst) V))
                     (contra (RestDots-ty rst))
                     (and rst (contra rst)))
                 (map contra kws)
                 (co rng)
                 rng-T+)]
    [(DepFun: dom pre rng)
     (make-DepFun (map contra dom) (contra pre) (co rng))]
    ;; the type in a NotTypeProp is underneath a negation
    [(NotTypeProp: obj t) (-not-type obj (contra t))]
    [(app Rep-variances (? pair? variances))
     (change-parts (Rep-constructor cur) (Rep-values cur) variances)]
    [(App: rator rands)
     (change-parts (λ rands (make-App rator rands))
                   rands
                   (or (app-variances rator rands)
                       (map (λ (_) variance:inv) rands)))]
    [(? invariant-type?) (if (mentions-V? cur) (give-up) cur)]
    [_ (Rep-fmap cur co)]))
