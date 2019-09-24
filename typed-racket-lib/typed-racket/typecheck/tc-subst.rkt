#lang racket/base

;; Functions in this file implement the substitution function in
;; figure 8, pg 8 of "Logical Types for Untyped Languages"

(require "../utils/utils.rkt"
         (utils tc-utils)
         racket/match
         (contract-req)
         (env lexical-env)
         (except-in (types abbrev utils prop-ops subtype
                           path-type subtract overlap)
                    -> ->* one-of/c)
         (only-in (infer infer) intersect restrict)
         (rep core-rep type-rep object-rep
              prop-rep rep-utils values-rep))

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
 [erase-identifiers (-> tc-results/c
                        (listof identifier?)
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
      [(Result: t ps o) (-tc-result t ps o)]))
  
  (match (instantiate-obj+simplify v subst)
    [(AnyValues: p)
     (-tc-any-results p)]
    [(Values: rs)
     (-tc-results (map res->tc-res rs) #f)]
    [(ValuesDots: rs dty dbound)
     (-tc-results (map res->tc-res rs) (make-RestDots dty dbound))]))

(define (erase-identifiers res names)
  (substitute-names res names (for/list ([_ (in-list names)])
                                -empty-obj)))

(define (instantiate-obj+simplify rep mapping)
  ;; lookup: if idx has a mapping,
  ;; then returns (cons/c OptObject? Type?),
  ;; else returns #f
  (define (lookup idx) (match (assv idx mapping)
                         [(cons _ entry) entry]
                         [_ #f]))
  (let subst/lvl ([rep rep] [lvl 0])
    (define (subst rep) (subst/lvl rep lvl))
    (match rep
      ;; Functions
      ;; increment the level of the substituted object
      [(Arrow: dom rst kws rng)
       (make-Arrow (map subst dom)
                   (and rst (subst rst))
                   (map subst kws)
                   (subst/lvl rng (add1 lvl)))]
      [(DepFun: dom pre rng)
       (make-DepFun (for/list ([d (in-list dom)])
                      (subst/lvl d (add1 lvl)))
                    (subst/lvl pre (add1 lvl))
                    (subst/lvl rng (add1 lvl)))]
      [(Intersection: ts raw-prop)
       (-refine (make-Intersection (map subst ts))
                (subst/lvl raw-prop (add1 lvl)))]
      [(Path: flds (cons (== lvl) (app lookup (cons o _))))
       (make-Path (map subst flds) o)]
      ;; restrict with the type for results and props
      [(TypeProp: (Path: flds (cons (== lvl) (app lookup (? pair? entry))))
                  prop-ty)
       (define o (make-Path (map subst flds) (car entry)))
       (define o-ty (or (path-type flds (cdr entry)) Univ))
       (define new-prop-ty (intersect prop-ty o-ty o))
       (cond
         [(Bottom? new-prop-ty) -ff]
         [(and (not (F? prop-ty))  (subtype o-ty prop-ty)) -tt]
         [(Empty? o) -tt]
         [else (-is-type o new-prop-ty)])]
      [(NotTypeProp: (Path: flds (cons (== lvl) (app lookup (? pair? entry))))
                     prop-ty)
       (define o (make-Path (map subst flds) (car entry)))
       (define o-ty (or (path-type flds (cdr entry)) Univ))
       (define new-o-ty (subtract o-ty prop-ty o))
       (define new-prop-ty (restrict prop-ty o-ty o))
       (cond
         [(or (Bottom? new-o-ty)
              (Univ? new-prop-ty))
          -ff]
         [(Empty? o) -tt]
         [else (-not-type o new-prop-ty)])]
      [(tc-result: orig-t
                   orig-ps
                   (Path: flds (cons (== lvl) (app lookup (? pair? entry)))))
       (define o (make-Path (map subst flds) (car entry)))
       (define t (intersect orig-t (or (path-type flds (cdr entry)) Univ)))
       (define ps (subst orig-ps))
       (-tc-result t ps o)]
      [(Result: orig-t
                orig-ps
                (Path: flds (cons (== lvl) (app lookup (? pair? entry)))))
       (define o (make-Path (map subst flds) (car entry)))
       (define t (intersect orig-t (or (path-type flds (cdr entry)) Univ)))
       (define ps (subst orig-ps))
       (make-Result t ps o)]
      ;; else default fold over subfields
      [_ (Rep-fmap rep subst)])))


