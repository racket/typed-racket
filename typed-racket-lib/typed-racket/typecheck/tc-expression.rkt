#lang racket/unit

(require
  "../utils/utils.rkt"
  (typecheck signatures tc-app-helper check-below)
  (types utils abbrev classes type-table)
  (rep type-rep measure-unit-rep)
  (utils tc-utils)
  (env index-env tvar-env scoped-tvar-env)
  (private syntax-properties parse-type)
  (base-env measures/parse-measure-unit)
  (for-template (only-in racket/base #%plain-app quote + - * / expt))
  racket/format
  racket/match
  syntax/stx
  syntax/parse)


(import tc-expr^ check-subforms^)
(export tc-expression^)

;; Typecheck an (#%expression e) form
(define (tc/#%expression form expected)
  (syntax-parse form
    [(exp:type-inst^ e)
     (do-inst (tc-expr #'e) (attribute exp.value))]
    [(exp:type-ascription^ e)
     (add-scoped-tvars #'e (parse-literal-alls (attribute exp.value)))
     (tc-expr/check #'e (parse-tc-results (attribute exp.value)))]
    [(exp:measure^ e)
     (add-measure-unit/tc-results (tc-expr #'e) (attribute exp.value))]
    [(exp:measure-arith^ e)
     (handle-measure-arith/tc-results #'e)]
    [(exp:ignore-some-expr^ e)
     (register-ignored! #'e)
     (check-subforms/ignore #'e)
     (fix-results (parse-tc-results (attribute exp.value)))]
    [(exp:external-check^ e)
     ((attribute exp.value) #'e)
     (if expected
        (tc-expr/check #'e expected)
        (tc-expr #'e))]
    [(_ e)
     (if expected
         (tc-expr/check #'e expected)
         (tc-expr #'e))]))

;; do-inst : tc-results? syntax? -> tc-results?
;; Perform a type instantiation, delegating to the appropriate helper
;; function depending on if the argument is a row or not
(define (do-inst tc-res inst)
  (define inst-type
    (if (row-syntax? inst) do-row-inst do-normal-inst))
  (define (error-case number)
    (tc-error/expr
      "Cannot instantiate expression that produces ~a values"
      number))
  (match tc-res
    [(tc-results: tys fs os)
     (match tys
      [(list ty)
       (ret (list (inst-type ty inst)) fs os)]
      [_ (error-case (if (null? tys) 0 "multiple"))])]
    [_ (error-case "multiple")]))


;; row-syntax? Syntax -> Boolean
;; This checks if the syntax object resulted from a row instantiation
(define (row-syntax? stx)
  (define lst (stx->list stx))
  (and lst (pair? lst)
       (eq? (syntax-e (car lst)) '#:row)))

;; do-normal-inst : Type Syntax -> Type
;; Instantiate a normal polymorphic type
(define (do-normal-inst ty inst)
  (cond
    [(not (or (Poly? ty) (PolyDots? ty)))
     (tc-error/expr #:return -Bottom "Cannot instantiate non-polymorphic type ~a"
                    (cleanup-type ty))]
    [(and (Poly? ty)
          (not (= (syntax-length inst) (Poly-n ty))))
     (tc-error/expr #:return -Bottom
                    "Wrong number of type arguments to polymorphic type ~a:\nexpected: ~a\ngot: ~a"
                    (cleanup-type ty) (Poly-n ty) (syntax-length inst))]
    [(and (PolyDots? ty) (not (>= (syntax-length inst) (sub1 (PolyDots-n ty)))))
     ;; we can provide 0 arguments for the ... var
     (tc-error/expr #:return -Bottom
                    "Wrong number of type arguments to polymorphic type ~a:\nexpected at least: ~a\ngot: ~a"
                    (cleanup-type ty) (sub1 (PolyDots-n ty)) (syntax-length inst))]
    [(PolyDots? ty)
     ;; In this case, we need to check the last thing.  If it's a dotted var, then we need to
     ;; use instantiate-poly-dotted, otherwise we do the normal thing.
     ;; In the case that the list is empty we also do the normal thing
     (match (syntax->list inst)
       [(list ty-stxs ... (app syntax-e (cons bound-ty-stx (? identifier? bound-id))))
        (unless (bound-index? (syntax-e bound-id))
          (tc-error/stx bound-id "~a is not a type variable bound with ..." (syntax-e bound-id)))
        (if (= (length ty-stxs) (sub1 (PolyDots-n ty)))
            (let* ([last-id (syntax-e bound-id)]
                   [last-ty (extend-tvars (list last-id) (parse-type bound-ty-stx))])
              (instantiate-poly-dotted ty (map parse-type ty-stxs) last-ty last-id))
            (tc-error/expr #:return -Bottom "Wrong number of fixed type arguments to polymorphic type ~a:\nexpected: ~a\ngot: ~a"
                           ty (sub1 (PolyDots-n ty)) (length ty-stxs)))]
       [stx-list
        (instantiate-poly ty (map parse-type stx-list))])]
    [else
     (instantiate-poly ty (stx-map parse-type inst))]))

;; do-row-inst : Type ClassRow -> Type
;; Instantiate a row polymorphic function type
(define (do-row-inst ty row-stx)
  ;; At this point, we know `stx` represents a row so we can parse it.
  ;; The parsing is done here because if `inst` did the parsing, it's
  ;; too early and ends up with an empty type environment.
  (define row
    (syntax-parse row-stx
      [(#:row (~var clauses (row-clauses parse-type)))
       (attribute clauses.row)]))
  (cond
    [(not (PolyRow? ty))
     (tc-error/expr #:return -Bottom "Cannot instantiate non-row-polymorphic type ~a"
                    (cleanup-type ty))]
    [else
     (match-define (PolyRow: _ constraints _) ty)
     (check-row-constraints
      row constraints
      (λ (name)
        (tc-error/expr
         (~a "Cannot instantiate row with member " name
             " that the given row variable requires to be absent"))))
     (instantiate-poly ty (list row))]))

;; add-measure-unit/tc-results : tc-results? syntax? -> tc-results?
(define (add-measure-unit/tc-results tc-res u)
  (define (error-case number)
    (tc-error/expr
      "Cannot instantiate expression that produces ~a values"
      number))
  (match tc-res
    [(tc-results: tys fs os)
     (match tys
      [(list ty)
       (ret (list (add-measure-unit ty u)) fs os)]
      [_ (error-case (if (null? tys) 0 "multiple"))])]
    [_ (error-case "multiple")]))

;; add-measure-unit : Type Syntax -> Type
(define (add-measure-unit ty u)
  (-Measure ty (parse-measure-unit u)))

;; handle-measure-arith/tc-results : syntax? -> tc-results?
(define (handle-measure-arith/tc-results stx)
  (syntax-parse stx #:literals (#%plain-app quote + - * / expt)
    [(#%plain-app * ~! a:expr ...)
     (match-define (list (tc-results: (list tys) _ _) ...) (stx-map tc-expr #'(a ...)))
     (match-define (list (Measure: ts us) ...) tys)
     (match-define (tc-results: (list n-ty) fs os) (tc-expr stx))
     (ret (list (-Measure n-ty (apply u* us))) fs os)]
    [(#%plain-app expt ~! a:expr (quote b:integer))
     (match-define (tc-results: (list ty) _ _) (tc-expr #'a))
     (match-define (Measure: t u) ty)
     (match-define (tc-results: (list n-ty) fs os) (tc-expr stx))
     (ret (list (-Measure n-ty (u^ u (syntax-e #'b)))) fs os)]
    [(#%plain-app + ~! a:expr ...+)
     (match-define (list (tc-results: (list tys) _ _) ...) (stx-map tc-expr #'(a ...)))
     (match-define (list (Measure: ts us) ...) tys)
     (match-define (tc-results: (list n-ty) fs os) (tc-expr stx))
     (match-define (list-rest u us*) us)
     (for ([u* (in-list us*)])
       (unless (measure-unit=? u u*)
         (tc-error/expr "m+ mismatched units: cannot add ~v and ~v" u u*)))
     (ret (list (-Measure n-ty u)) fs os)]
    [_ (tc-error/expr "unrecognized measure arithmetic form")]))

