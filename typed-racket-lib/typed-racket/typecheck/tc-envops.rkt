#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer infer-in]))
(require racket/match racket/list
         (only-in unstable/list list-update)
         (for-syntax racket/base syntax/parse)
         (contract-req)
         (infer-in infer)
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         (types tc-result resolve subtype remove-intersect union filter-ops)
         (env type-env-structs lexical-env)
         (rename-in (types abbrev)
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (typecheck tc-metafunctions))

(provide with-lexical-env/extend-props)


(define/cond-contract (update t ft pos? lo)
  (Type/c Type/c boolean? (listof PathElem?) . -> . Type/c)
  ;; build-type: build a type while propogating bottom
  (define (build-type constructor . args)
    (if (memf Bottom? args) -Bottom (apply constructor args)))
  (match* ((resolve t) lo)
    ;; pair ops
    [((Pair: t s) (list rst ... (CarPE:)))
     (build-type -pair (update t ft pos? rst) s)]
    [((Pair: t s) (list rst ... (CdrPE:)))
     (build-type -pair t (update s ft pos? rst))]

    ;; syntax ops
    [((Syntax: t) (list rst ... (SyntaxPE:)))
     (build-type -Syntax (update t ft pos? rst))]

    ;; promise op
    [((Promise: t) (list rst ... (ForcePE:)))
     (build-type -Promise (update t ft pos? rst))]

    ;; struct ops
    [((Struct: nm par flds proc poly pred)
      (list rst ... (StructPE: (? (lambda (s) (subtype t s)) s) idx)))
     ;; note: this updates fields regardless of whether or not they are
     ;; a polymorphic field. Because subtyping is nominal and accessor 
     ;; functions do not reflect this, this behavior is unobservable
     ;; except when an a variable aliases the field in a let binding
     (let*-values ([(lhs rhs) (split-at flds idx)]
                   [(ty* acc-id) (match rhs
                                   [(cons (fld: ty acc-id #f) _)
                                    (values (update ty ft pos? rst) acc-id)]
                                   [_ (int-err "update on mutable struct field")])]) 
       (cond 
        [(Bottom? ty*) ty*]
        [else (let ([flds* (append lhs (cons (make-fld ty* acc-id #f) (cdr rhs)))])
                (make-Struct nm par flds* proc poly pred))]))]

    ;; otherwise
    [(t '())
     (if pos?
         (restrict t ft)
         (remove t ft))]
    [((Union: ts) lo)
     (apply Un (map (λ (t) (update t ft pos? lo)) ts))]
    [(t* lo)
     ;; This likely comes up with (-lst t) and we need to improve the system to make sure this case
     ;; dosen't happen
     ;;(int-err "update along ill-typed path: ~a ~a ~a" t t* lo)
     t]))

;; Returns #f if anything becomes (U)
(define (env+ env fs)
  (let/ec exit*
    (define (exit) (exit* #f empty))
    (define-values (props atoms) (combine-props fs (env-props env) exit))
    (values
      (for/fold ([Γ (replace-props env props)]) ([f (in-list atoms)])
        (match f
          [(or (TypeFilter: ft (Path: lo x)) (NotTypeFilter: ft (Path: lo x)))
           (update-type/lexical
             (lambda (x t)
               (define new-t (update t ft (TypeFilter? f) lo))
               (when (type-equal? new-t -Bottom)
                 (exit))
               new-t)
             x Γ)]
          [_ Γ]))
      atoms)))

;; run code in an extended env and with replaced props. Requires the body to return a tc-results.
;; TODO make this only add the new prop instead of the entire environment once tc-id is fixed to
;; include the interesting props in its filter.
;; WARNING: this may bail out when code is unreachable
(define-syntax (with-lexical-env/extend-props stx)
  (define-splicing-syntax-class unreachable?
    (pattern (~seq #:unreachable form:expr))
    (pattern (~seq) #:with form #'(begin)))
  (syntax-parse stx
    [(_ ps:expr u:unreachable? . b)
     #'(let-values ([(new-env atoms) (env+ (lexical-env) ps)])
         (if new-env
             (with-lexical-env new-env
               (add-unconditional-prop (let () . b) (apply -and (append atoms (env-props new-env)))))
             ;; unreachable, bail out
             (let ()
               u.form
               (ret -Bottom))))]))
