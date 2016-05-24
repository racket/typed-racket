#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer infer-in]))
(require racket/match racket/list
         (contract-req)
         (infer-in infer)
         (rep type-rep prop-rep object-rep rep-utils)
         (utils tc-utils)
         (types resolve subtype remove union)
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
  (Type/c Type/c boolean? (listof PathElem?) . -> . Type/c)
  ;; build-type: build a type while propogating bottom
  (define (build constructor . args)
    (if (memf Bottom? args) -Bottom (apply constructor args)))
  ;; update's inner recursive loop
  ;; puts path in *accessed* order
  ;; (i.e. (car (cdr x)) --> (list cdr car))
  (let update
    ([t t] [path (reverse path-elems)])
    (match path
      ;; path is empty (base case)
      [(list) (cond
                [pos? (intersect (resolve t) new-t)]
                [else (remove (resolve t) new-t)])]
      ;; path is non-empty
      ;; (i.e. there is some field access we'll try and traverse)
      [(cons path-elem rst)
       (match* ((resolve t) path-elem)
         ;; pair ops
         [((Pair: t s) (CarPE:))
          (build -pair (update t rst) s)]
         [((Pair: t s) (CdrPE:))
          (build -pair t (update s rst))]
         ;; syntax ops
         [((Syntax: t) (SyntaxPE:))
          (build -Syntax (update t rst))]
         ;; promise op
         [((Promise: t) (ForcePE:))
          (build -Promise (update t rst))]
         
         ;; struct ops
         [((Struct: nm par flds proc poly pred)
           (StructPE: (? (λ (s) (subtype t s))) idx))
          ;; Note: this updates fields even if they are not polymorphic.
          ;; Because subtyping is nominal and accessor functions do not
          ;; reflect this, this behavior is unobservable except when a
          ;; variable aliases the field in a let binding
          (define-values (lhs rhs) (split-at flds idx))
          (define-values (ty* acc-id)
            (match rhs
              [(cons (fld: ty acc-id #f) _)
               (values (update ty rst) acc-id)]
              [_ (int-err "update on mutable struct field")]))
          (cond 
            [(Bottom? ty*) ty*]
            [else
             (define flds*
               (append lhs (cons (make-fld ty* acc-id #f) (cdr rhs))))
             (make-Struct nm par flds* proc poly pred)])]
         
         ;; class field ops
         ;;
         ;; A refinement of a private field in a class is really a refinement of the
         ;; return type of the accessor function for that field (rather than a variable).
         ;; We cannot just refine the type of the argument to the accessor, since that
         ;; is an object type that doesn't mention private fields. Thus we use the
         ;; FieldPE path element as a marker to refine the result of the accessor
         ;; function.
         [((Function: (list (arr: doms (Values: (list (Result: rng _ _))) _ _ _)))
           (FieldPE:))
          (make-Function
           (list (make-arr* doms (update rng rst))))]
         
         [((Union: ts) _)
          (apply Un (map (λ (t) (update t path)) ts))]
         
         [(_ _)
          ;; This likely comes up with (-lst t) and we need to improve the system to make sure this case
          ;; dosen't happen
          ;;(int-err "update along ill-typed path: ~a ~a ~a" t t* lo)
          t])])))