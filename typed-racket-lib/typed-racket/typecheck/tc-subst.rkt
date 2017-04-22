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
                    tc-results/c)]
 [erase-names (-> (listof identifier?)
                  tc-results/c
                  tc-results/c)])

(provide subst-rep
         instantiate-rep/obj)


(define (instantiate-rep/obj rep obj [t Univ])
  (subst-rep rep (list (list (cons 0 0) obj t))))


;; Substitutes the given objects into the values and turns it into a
;; tc-result.  This matches up to the substitutions in the T-App rule
;; from the ICFP paper.
(define (values->tc-results v os [ts '()])
  (define targets
    (for/list ([o (in-list os)]
               [arg (in-naturals)]
               [t (in-list/rest ts Univ)])
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
;; arguments to be of the types ts. Uses an identity substitution (yuck)
;; since substitution does this same restriction.
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

(define (erase-names names res)
  (define targets
    (for/list ([nm (in-list names)])
      (list nm -empty-obj Univ)))
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


;; inc-lvls
;; This function increments the 'lvl' field in all of the targets
;; and objects of substitution (see 'inc-lvl' above)
(define (inc-lvls targets)
  (for/list ([tgt (in-list targets)])
    (match tgt
      [(list nm1 (Path: flds nm2) ty)
       (list (inc-lvl nm1) (make-Path flds (inc-lvl nm2)) ty)]
      [(cons nm1 rst)
       (cons (inc-lvl nm1) rst)])))

;; Simple substitution of objects into a Rep
;; This is basically 'rep[x ↦ o]'.
;; If that was the only substitution we were doing,
;; and the type of 'x' was 'τ', then 'targets'
;; would equal (list (list x o τ)) (i.e. it's the list of
;; identifiers being substituted out, the optional object replacing
;; them, and their type).
(define/cond-contract (subst-rep rep targets)
  (-> any/c (listof (list/c name-ref/c OptObject? Type?))
      any/c)
  (define (subst/inc rep)
    (subst-rep rep (inc-lvls targets)))
  ;; substitution loop
  (let subst ([rep rep])
    (match rep
      ;; Functions
      ;; increment the level of the substituted object
      [(arr: dom rng rest drest kws)
       (make-arr (map subst dom)
                 (subst/inc rng)
                 (and rest (subst rest))
                 (and drest (cons (subst (car drest)) (cdr drest)))
                 (map subst kws))]
      [(Intersection: ts raw-prop)
       (-refine (make-Intersection (map subst ts))
                (subst/inc raw-prop))]
      [(Path: flds nm)
       (let ([flds (map subst flds)])
         (cond
           [(assoc nm targets name-ref=?)
            =>
            (λ (l) (match (second l)
                     [(Empty:) -empty-obj]
                     [(Path: flds* nm*)
                      (make-Path (append flds flds*) nm*)]
                     [(? LExp? l) #:when (null? flds) l]
                     [_ -empty-obj]))]
           [else (make-Path flds nm)]))]
      ;; restrict with the type for results and props
      [(TypeProp: (and obj (Path: flds nm)) obj-ty)
       (let ([flds (map subst flds)])
         (cond
           [(assoc nm targets name-ref=?) =>
            (match-lambda
              [(list _ inner-obj inner-obj-ty)
               (define inner-obj-ty-at-flds (or (path-type flds inner-obj-ty) Univ))
               (define new-obj-ty (intersect obj-ty inner-obj-ty-at-flds obj))
               (match inner-obj
                 [_ #:when (Bottom? new-obj-ty) -ff]
                 [_ #:when (subtype inner-obj-ty-at-flds obj-ty) -tt]
                 [(Empty:) -tt]
                 [(Path: flds* nm*)
                  (define resulting-obj (make-Path (append flds flds*) nm*))
                  (-is-type resulting-obj new-obj-ty)]
                 [(? LExp? l) (if (null? flds)
                                  (-is-type l new-obj-ty)
                                  -ff)])])]
           [else (-is-type (make-Path flds nm) (subst obj-ty))]))]
      [(NotTypeProp: (and obj (Path: flds nm)) neg-obj-ty)
       (let ([flds (map subst flds)])
         (cond
           [(assoc nm targets name-ref=?)
            =>
            (match-lambda
              [(list _ inner-obj inner-obj-ty)
               (define inner-obj-ty-at-flds (or (path-type flds inner-obj-ty) Univ))
               (define new-obj-ty (subtract inner-obj-ty-at-flds neg-obj-ty obj))
               (define new-neg-obj-ty (restrict neg-obj-ty inner-obj-ty-at-flds obj))
               (match inner-obj
                 [_ #:when (Bottom? new-obj-ty) -ff]
                 [_ #:when (Bottom? new-neg-obj-ty) -tt]
                 [(Empty:) -tt]
                 [(Path: flds* nm*)
                  (define resulting-obj (make-Path (append flds flds*) nm*))
                  (-not-type resulting-obj new-neg-obj-ty)]
                 [(? LExp? l) (if (null? flds)
                                  (-not-type l new-neg-obj-ty)
                                  -ff)])])]
           [else
            (-not-type (make-Path flds nm) (subst neg-obj-ty))]))]
      ;; else default fold over subfields
      [_ (Rep-fmap rep subst)])))
