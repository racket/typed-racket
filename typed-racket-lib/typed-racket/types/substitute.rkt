#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/set
         racket/lazy-require
         (contract-req)
         (only-in (types base-abbrev) -Tuple* -lst -Null -result ManyUniv)
         (rep type-rep values-rep rep-utils)
         (utils tc-utils)
         (rep rep-utils free-variance)
         (env tvar-env))
(lazy-require ("union.rkt" (Un)))

(provide subst-all substitute substitute-dots substitute-dotted subst
         (struct-out t-subst) (struct-out i-subst)
         (struct-out i-subst/starred) (struct-out i-subst/dotted)
         make-simple-substitution)
(provide-for-cond-contract substitution/c)

(define-struct/cond-contract subst-rhs () #:transparent)
(define-struct/cond-contract (t-subst subst-rhs) ([type Rep?]) #:transparent)
(define-struct/cond-contract (i-subst subst-rhs) ([types (listof Rep?)]) #:transparent)
(define-struct/cond-contract (i-subst/starred subst-rhs) ([types (listof Rep?)] [starred Rep?]) #:transparent)
(define-struct/cond-contract (i-subst/dotted subst-rhs) ([types (listof Rep?)] [dty Rep?] [dbound symbol?]) #:transparent)

(define-for-cond-contract substitution/c (hash/c symbol? subst-rhs? #:immutable #t))
(define-for-cond-contract simple-substitution/c (hash/c symbol? Rep? #:immutable #t))

(define (subst v t e) (substitute t v e))

(define/cond-contract (make-simple-substitution vs ts)
  (([vs (listof symbol?)] [ts (listof Rep?)]) ()
   #:pre (vs ts) (= (length vs) (length ts))
    . ->i . [_ substitution/c])
  (for/hash ([v (in-list vs)] [t (in-list ts)])
    (values v (t-subst t))))

;; TODO: Figure out if free var checking/short circuiting is actually a performance improvement.

;; substitute-many : Hash[Name,Type] Type -> Type
(define/cond-contract (substitute-many subst target)
  (simple-substitution/c Rep? . -> .  Rep?)
  (define names (hash-keys subst))
  (let sub ([target target])
    (match target
      [(F: name) (hash-ref subst name target)]
      [(or (RestDots: _     dbound)
           (ListDots: _     dbound)
           (ValuesDots: _ _ dbound))
       #:when (and (memq dbound names)
                   (not (bound-tvar? dbound)))
       (int-err "substitute used on ... variable ~a in type ~a"
                dbound
                target)]
      [_ (Rep-fmap target sub)])))



;; substitute : Type Name Type -> Type
(define/cond-contract (substitute image name target)
  (Rep? symbol? Rep? . -> . Rep?)
  (substitute-many (hash name image) target))

;; implements angle bracket substitution from the formalism
;; substitute-dots : Listof[Type] Option[type] Name Type -> Type
;; implements angle bracket substitution from the formalism (TODO what formalism?)
;; substitute-dots : Listof[Type] Option[type] Name Type -> Type
(define/cond-contract (substitute-dots images rimage name target)
  ((listof Rep?) (or/c #f Rep?) symbol? Rep?  . -> . Rep?)
  (let sub ([target target])
    (match target
      [(ListDots: dty dbound)
       (if (eq? name dbound)
           ;; We need to recur first, just to expand out any dotted usages of this.
           (let ([expanded (sub dty)])
             (for/fold ([t (if rimage (-lst rimage) -Null)])
                       ([img (in-list (reverse images))])
               (make-Pair (substitute img name expanded) t)))
           (make-ListDots (sub dty) dbound))]
      [(ValuesDots: types dty dbound)
       (cond
         [(eq? name dbound)
          (cond
            [rimage ManyUniv]
            [else
             (make-Values
              (append
               (map sub types)
               ;; We need to recur first, just to expand out any dotted usages of this.
               (let ([expanded (sub dty)])
                 (for/list ([img (in-list images)])
                   (-result (substitute img name expanded))))))])]
         [else (make-ValuesDots (map sub types) (sub dty) dbound)])]
      [(Arrow: dom (RestDots: dty dbound) kws rng)
       #:when (eq? name dbound)
       (make-Arrow
        (append
         (map sub dom)
         ;; We need to recur first, just to expand out any dotted usages of this.
         (let ([expanded (sub dty)])
           (map (λ (img) (substitute img name expanded))
                images)))
        (if (Type? rimage) (make-Rest (list rimage)) rimage)
        (map sub kws)
        (sub rng))]
      [_ (Rep-fmap target sub)])))

;; implements curly brace substitution from the formalism, with the addition
;; that a substitution can include fixed args in addition to a different dotted arg
;; substitute-dotted : Listof[Type] Type Name Name Type -> Type
(define (substitute-dotted pre-image image image-bound name target)
  (let sub ([target target])
    (match target
      [(F: name*) #:when (eq? name* name) image]
      [(ValuesDots: types dty dbound)
       #:when (eq? name dbound)
       (make-ValuesDots (append (map sub types)
                                (map -result pre-image))
                        (sub dty)
                        image-bound)]
      [(ListDots: dty dbound)
       (-Tuple*
        (if (eq? name dbound) pre-image null)
        (make-ListDots (sub dty)
                       (if (eq? name dbound) image-bound dbound)))]
      [(Arrow: dom (RestDots: dty dbound) kws rng)
       #:when (eq? name dbound)
       (make-Arrow
        (append (map sub dom) pre-image)
        (make-RestDots
         (substitute image dbound (sub dty))
         image-bound)
        (map sub kws)
        (sub rng))]
      [_ (Rep-fmap target sub)])))

;; substitute many variables
;; subst-all : substitution/c Type -> Type
(define/cond-contract (subst-all s ty)
  (substitution/c Rep? . -> . Rep?)

  (define t-substs
    (for/fold ([acc (hash)]) ([(v r) (in-hash s)])
      (match r
        [(t-subst img)
         (hash-set acc v img)]
        [_ acc])))
  (define t-substed-ty (substitute-many t-substs ty))

  (for/fold ([t t-substed-ty]) ([(v r) (in-hash s)])
    (match r
      [(t-subst img) t]
      [(i-subst imgs)
       (substitute-dots imgs #f v t)]
      [(i-subst/starred imgs rest)
       (substitute-dots imgs rest v t)]
      [(i-subst/dotted imgs dty dbound)
       (substitute-dotted imgs dty dbound v t)])))
