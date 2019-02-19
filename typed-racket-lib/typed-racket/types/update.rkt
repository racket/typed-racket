#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer infer-in]))
(require racket/match racket/list
         (contract-req)
         (infer-in infer)
         (rep core-rep type-rep prop-rep object-rep values-rep rep-utils)
         (utils tc-utils prefab)
         (types resolve subtype subtract)
         (rename-in (types abbrev)
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c]))

(provide update)

;; update
;; t          : type being updated
;; new-t      : new type
;; pos?       : whether the update is positive or negative
;; path-elems : which fields we're traversing to update,
;;   in *syntactic order* (e.g. (car (cdr x)) -> '(car cdr))
(define/cond-contract (update t new-t pos? path-elems)
  (Type? Type? boolean? (listof PathElem?) . -> . Type?)
  ;; update's inner recursive loop
  ;; puts path in *accessed* order
  ;; (i.e. (car (cdr x)) --> (list cdr car))
  (let update
    ([t t] [path (reverse path-elems)])
    (match path
      ;; path is non-empty
      ;; (i.e. there is some field access we'll try and traverse)
      [(cons path-elem rst)
       (match* ((resolve t) path-elem)
         ;; pair ops
         [((Pair: t s) (CarPE:))
          (rebuild -pair (update t rst) s)]
         [((Pair: t s) (CdrPE:))
          (rebuild -pair t (update s rst))]
         ;; syntax ops
         [((Syntax: t) (SyntaxPE:))
          (rebuild -Syntax (update t rst))]
         ;; promise op
         [((Promise: t) (ForcePE:))
          (rebuild -Promise (update t rst))]

         ;; struct ops
         ;; (NOTE: we assume path elements to mutable fields
         ;;        are never created)
         [((Struct: nm par flds proc poly pred props)
           (StructPE: s idx))
          #:when (subtype t s)
          ;; Note: this updates fields even if they are not polymorphic.
          ;; Because subtyping is nominal and accessor functions do not
          ;; reflect this, this behavior is unobservable except when a
          ;; variable aliases the field in a let binding
          (match-define-values (lhs (cons (fld: ty acc-id #f) rhs)) (split-at flds idx))
          (match (update ty rst)
            [(Bottom:) -Bottom]
            [ty (let ([flds (append lhs (cons (make-fld ty acc-id #f) rhs))])
                  (make-Struct nm par flds proc poly pred props))])]

         ;; prefab struct ops
         ;; (NOTE: we assume path elements to mutable fields
         ;;        are never created)
         [((Prefab: key flds) (PrefabPE: path-key idx))
          #:when (prefab-key-subtype? key path-key)
          (match-define-values (lhs (cons fld-ty rhs)) (split-at flds idx))
          (match (update fld-ty rst)
            [(Bottom:) -Bottom]
            [fld-ty (make-Prefab key (append lhs (cons fld-ty rhs)))])]

         ;; class field ops
         ;;
         ;; A refinement of a private field in a class is really a refinement of the
         ;; return type of the accessor function for that field (rather than a variable).
         ;; We cannot just refine the type of the argument to the accessor, since that
         ;; is an object type that doesn't mention private fields. Thus we use the
         ;; FieldPE path element as a marker to refine the result of the accessor
         ;; function.
         [((Fun: (list (Arrow: doms _ _ (Values: (list (Result: rng _ _))))))
           (FieldPE:))
          (make-Fun (list (-Arrow doms (update rng rst))))]

         [((Union: _ ts) _)
          ;; Note: if there is a path element, then all Base types are
          ;; incompatible with the type and we can therefore drop the
          ;; bases from the union
          (Union-fmap (λ (t) (update t path)) -Bottom ts)]

         [((Intersection: ts raw-prop) _)
          (-refine (for/fold ([t Univ])
                             ([elem (in-list ts)])
                     (intersect t (update elem path)))
                   raw-prop)]

         [(_ _)
          (match path-elem
            [(CarPE:) (intersect t (-pair (update Univ rst) Univ))]
            [(CdrPE:) (intersect t (-pair Univ (update Univ rst)))]
            [(SyntaxPE:) (intersect t (-syntax-e (update Univ rst)))]
            [(ForcePE:) (intersect t (-force (update Univ rst)))]
            [(PrefabPE: key idx)
             #:when (not (prefab-key/mutable-fields? key))
             (define field-count (prefab-key->field-count key))
             (define updated-field (update Univ rst))
             (define fields (for/list ([fld-idx (in-range field-count)])
                              (if (eqv? idx fld-idx)
                                  updated-field
                                  Univ)))
             (intersect t (make-Prefab key fields))]
            [_ t])])]
      ;; path is empty (base case)
      [_ (cond
           [pos? (intersect (resolve t) new-t)]
           [else (subtract (resolve t) new-t)])])))
