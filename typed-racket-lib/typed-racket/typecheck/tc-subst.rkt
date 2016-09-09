#lang racket/base

;; Functions in this file implement the substitution function in
;; figure 8, pg 8 of "Logical Types for Untyped Languages"

(require "../utils/utils.rkt"
         (utils tc-utils)
         racket/match racket/list
         (contract-req)
         (except-in (types abbrev utils prop-ops path-type subtract overlap)
                    -> ->* one-of/c)
         (only-in (infer infer) intersect restrict)
         (types subtype)
         (rep core-rep type-rep object-rep
              prop-rep rep-utils values-rep))

(provide/cond-contract
 [restrict-values (-> SomeValues? (listof Type?) SomeValues?)]
 [values->tc-results (->* (SomeValues? (listof OptObject?))
                          ((listof Type?))
                          full-tc-results/c)]
 [replace-names (-> (listof identifier?)
                    (listof OptObject?)
                    tc-results/c
                    tc-results/c)])


;; Substitutes the given objects into the values and turns it into a
;; tc-result.  This matches up to the substitutions in the T-App rule
;; from the ICFP paper.
(define (values->tc-results v os [ts (map (λ (_) Univ) os)])
  (define targets
    (for/list ([o (in-list os)]
               [arg (in-naturals)]
               [t (in-list ts)])
      (list (cons 0 arg) o t)))
  (define res
    (match v
      [(AnyValues: p)
       (tc-any-results p)]
      [(Results: t ps o)
       (ret t ps o)]
      [(Results: t ps o dty dbound)
       (ret t ps o dty dbound)]
      [_ (int-err "invalid res in values->tc-results: ~a" res)]))
  
  (subst-tc-results res targets))

;; Restrict the objects in v refering to the current functions
;; arguments to be of the types ts.
(define (restrict-values v ts)
  (define targets
    (for/list ([t (in-list ts)]
               [arg (in-naturals)])
      (define nm (cons 0 arg))
      (list nm (-id-path nm) t)))
  (subst-rep v targets))


;; For each name replaces all uses of it in res with the
;; corresponding object.  This is used so that names do not escape the
;; scope of their definitions
(define (replace-names names objects res)
  (define targets
    (for/list ([nm (in-list names)]
               [o (in-list objects)])
      (list nm o Univ)))
  (subst-tc-results res targets))

(define (subst-tc-results res targets)
  (define (sr t ps o)
    (subst-tc-result t ps o targets))
  (define (sub x) (subst-rep x targets))
  
  (match res
    [(tc-any-results: p) (tc-any-results (sub p))]
    [(tc-results: ts ps os)
     (tc-results (map sr ts ps os) #f)]
    [(tc-results: ts ps os dt db)
     (tc-results (map sr ts ps os) (cons (sub dt) db))]
    [_ (int-err "invalid res in subst-tc-results: ~a" res)]))

;; Substitution of objects into a tc-result This is a combination of
;; the other substitutions, plus a restriction of the returned type to
;; the arguments type if the returned object corresponds to an
;; argument.
(define (subst-tc-result r-t r-ps r-o targets)
  (define type*
    (match r-o
      [(Path: flds nm)
       (cond
         [(assoc nm targets name-ref=?) =>
          (match-lambda
            [(list _ _ t)
             (or (path-type flds t) Univ)])]
         [else Univ])]
      [_ Univ]))

  (tc-result
   (intersect (subst-rep r-t targets)
              type*)
   (subst-rep r-ps targets)
   (subst-rep r-o targets)))


(define (inc-lvl x)
  (match x
    [(cons lvl arg) (cons (add1 lvl) arg)]
    [_ x]))

(define (inc-lvls targets)
  (for/list ([tgt (in-list targets)])
    (match tgt
      [(list nm1 (Path: flds nm2) ty)
       (list (inc-lvl nm1) (make-Path flds (inc-lvl nm2)) ty)]
      [(cons nm1 rst)
       (cons (inc-lvl nm1) rst)])))

;; Simple substitution of objects into a Rep
;; This is 'rep[x ↦ o]'
(define/cond-contract (subst-rep rep targets)
  (-> any/c (listof (list/c name-ref/c OptObject? Type?))
      any/c)
  (define (sub/inc rep)
    (subst-rep rep (inc-lvls targets)))
  ;; substitution loop
  (let subst ([rep rep])
    (match rep
      ;; Functions
      ;; increment the level of the substituted object
      [(arr: dom rng rest drest kws)
       (make-arr (map subst dom)
                 (sub/inc rng)
                 (and rest (subst rest))
                 (and drest (cons (subst (car drest)) (cdr drest)))
                 (map subst kws))]
      [(Path: flds nm)
       (let ([flds (map subst flds)])
         (cond
           [(assoc nm targets name-ref=?) =>
            (λ (l) (match (second l)
                     [(Empty:) -empty-obj]
                     [(Path: flds* nm*)
                      (make-Path (append flds flds*) nm*)]))]
           [else (make-Path flds nm)]))]
      ;; restrict with the type for results and props
      [(TypeProp: (Path: flds nm) ty-at-flds)
       (let ([flds (map subst flds)])
         (cond
           [(assoc nm targets name-ref=?) =>
            (match-lambda
              [(list _ new-obj new-obj-ty)
               (define arg-ty-at-flds (or (path-type flds new-obj-ty) Univ))
               (define new-ty-at-flds (intersect ty-at-flds arg-ty-at-flds))
               (match new-obj
                 [_ #:when (Bottom? new-ty-at-flds) -ff]
                 [_ #:when (subtype arg-ty-at-flds ty-at-flds) -tt]
                 [(Empty:) -tt]
                 [(Path: flds* nm*)
                  (define resulting-obj (make-Path (append flds flds*) nm*))
                  (-is-type resulting-obj new-ty-at-flds)])])]
           [else (-is-type (make-Path flds nm) (subst ty-at-flds))]))]
      [(NotTypeProp: (Path: flds nm) not-ty-at-flds)
       (let ([flds (map subst flds)])
         (cond
           [(assoc nm targets name-ref=?) =>
            (match-lambda
              [(list _ new-obj new-obj-ty)
               (define arg-ty-at-flds (or (path-type flds new-obj-ty) Univ))
               (define new-ty-at-flds (subtract arg-ty-at-flds not-ty-at-flds))
               (define new-not-ty-at-flds (restrict not-ty-at-flds arg-ty-at-flds))
               (match new-obj
                 [_ #:when (Bottom? new-ty-at-flds) -ff]
                 [_ #:when (Bottom? new-not-ty-at-flds) -tt]
                 [(Empty:) -tt]
                 [(Path: flds* nm*)
                  (define resulting-obj (make-Path (append flds flds*) nm*))
                  (-not-type resulting-obj new-not-ty-at-flds)])])]
           [else
            (-not-type (make-Path flds nm) (subst not-ty-at-flds))]))]
      ;; else default fold over subfields
      [_ (Rep-fold subst rep)])))
