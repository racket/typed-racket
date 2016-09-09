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
      [(arr: dom rng rest drest kws)
       (cond
         [(and (pair? drest)
               (ormap (λ (name) (and (equal? name (cdr drest))
                                     (not (bound-tvar? name))
                                     name))
                      names))
          =>
          (λ (name)
            (int-err "substitute used on ... variable ~a in type ~a" name target))]
         [else
          (make-arr (map sub dom)
                    (sub rng)
                    (and rest (sub rest))
                    (and drest (cons (sub (car drest)) (cdr drest)))
                    (map sub kws))])]
      [(ValuesDots: types dty dbound)
       (cond
         [(for/or ([name (in-list names)])
            (and (equal? dbound name)
                 (not (bound-tvar? name))))
          =>
          (λ (name)
            (int-err "substitute used on ... variable ~a in type ~a" name target))]
         [else (make-ValuesDots (map sub types) (sub dty) dbound)])]
      [(ListDots: dty dbound)
       (cond
         [(for/or ([name (in-list names)])
            (and (equal? dbound name)
                 (not (bound-tvar? name))))
          =>
          (λ (name)
            (int-err "substitute used on ... variable ~a in type ~a" name target))]
         [else (make-ListDots (sub dty) dbound)])]
      [_ (Rep-fold sub target)])))



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
      [(arr: dom rng rest drest kws)
       (cond
         [(and (pair? drest)
               (eq? name (cdr drest)))
          (make-arr (append
                     (map sub dom)
                     ;; We need to recur first, just to expand out any dotted usages of this.
                     (let ([expanded (sub (car drest))])
                       (map (λ (img) (substitute img name expanded))
                            images)))
                    (sub rng)
                    rimage
                    #f
                    (map sub kws))]
         [else
          (make-arr (map sub dom)
                    (sub rng)
                    (and rest (sub rest))
                    (and drest (cons (sub (car drest)) (cdr drest)))
                    (map sub kws))])]
      [_ (Rep-fold sub target)])))

;; implements curly brace substitution from the formalism, with the addition
;; that a substitution can include fixed args in addition to a different dotted arg
;; substitute-dotted : Listof[Type] Type Name Name Type -> Type
(define (substitute-dotted pre-image image image-bound name target)
  (let sub ([target target])
    (match target
      [(ValuesDots: types dty dbound)
       (let ([extra-types (cond
                            [(eq? name dbound) pre-image]
                            [else null])])
         (make-ValuesDots (append (map sub types) (map -result extra-types))
                          (sub dty)
                          (cond
                            [(eq? name dbound) image-bound]
                            [else dbound])))]
      [(ListDots: dty dbound)
       (-Tuple*
        (if (eq? name dbound) pre-image null)
        (make-ListDots (sub dty)
                       (if (eq? name dbound) image-bound dbound)))]
      [(F: name*)
       (cond [(eq? name* name) image]
             [else target])]
      [(arr: dom rng rest drest kws)
       (let ([extra-types (cond
                            [(and drest (eq? name (cdr drest)))
                             pre-image]
                            [else null])])
         (make-arr (append (map sub dom) extra-types)
                   (sub rng)
                   (and rest (sub rest))
                   (and drest
                        (cons (substitute image (cdr drest) (sub (car drest)))
                              (cond
                                [(eq? name (cdr drest))
                                 image-bound]
                                [else (cdr drest)])))
                   (map sub kws)))]
      [_ (Rep-fold sub target)])))

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
