#lang racket/unit

(require
  "../utils/utils.rkt"
  (typecheck signatures possible-domains check-below)
  (types utils abbrev classes type-table)
  (rep type-rep)
  (utils tc-utils)
  (env index-env tvar-env scoped-tvar-env)
  (private syntax-properties parse-type)
  racket/format
  racket/match
  syntax/stx
  syntax/parse)


(import tc-expr^ check-subforms^)
(export tc-expression^)

;; Typecheck an (#%expression e) form
(define (tc/#%expression form [expected #f])
  (syntax-parse form
    [(exp:type-inst^ e)
     (do-inst (tc-expr #'e) (attribute exp.value))]
    [(exp:row-inst^ e)
     (do-inst (tc-expr #'e) (attribute exp.value) #t)]
    [(exp:ignore-some-expr^ e)
     (register-ignored! #'e)
     (check-subforms/ignore #'e)
     (fix-results (parse-tc-results (attribute exp.value)))]
    [(exp:external-check^ e)
     ((attribute exp.value) #'e)
     (if expected
        (tc-expr/check #'e expected)
        (tc-expr #'e))]
    [(exp:casted-expr^ e)
     (define result (tc-expr #'e))
     (match result
       [(tc-result1: ty)
        ((attribute exp.value) ty)
        result]
       [_
        (tc-error/expr "Cannot cast expression that produces multiple values")])]
    [(_ e)
     (if expected
         (tc-expr/check #'e expected)
         (tc-expr #'e))]))

;; do-inst : tc-results? syntax? -> tc-results?
;; Perform a type instantiation, delegating to the appropriate helper
;; function depending on if the argument is a row or not
(define (do-inst tc-res inst [row? #f])
  (define inst-type
    (if row? do-row-inst do-normal-inst))
  (define (error-case number)
    (tc-error/expr
     "Cannot instantiate expression that produces ~a values"
     number))
  (match tc-res
    [(tc-results: (list (tc-result: t ps o)) #f)
     ;; we erase 'o' -- if they bothered to put an instantiation,
     ;; odds are this is not something where 'o' matters, and leaving
     ;; 'o' can cause complications (see TR gh issue 561) -- maybe there's
     ;; a better way? this seems totally fine for now
     (ret (inst-type t inst) ps -empty-obj)]
    [_ (error-case (if (and (tc-results? tc-res)
                            (null? (tc-results-ts tc-res)))
                       0
                       "multiple"))]))


;; do-normal-inst : Type Syntax -> Type
;; Instantiate a normal polymorphic type
(define (do-normal-inst ty inst)
  (cond
    [(not (or (Poly? ty) (PolyDots? ty)))
     (tc-error/expr #:return -Bottom "Cannot instantiate non-polymorphic type ~a"
                    (cleanup-type ty))]
    [(and (Poly? ty)
          (> (syntax-length inst) (Poly-n ty)))
     (tc-error/expr #:return -Bottom
                    "Too many type arguments to polymorphic type ~a:\nexpected ~a or fewer\ngot: ~a"
                    (cleanup-type ty) (Poly-n ty) (syntax-length inst))]
    [(and (PolyDots? ty)
          (not (>= (syntax-length inst) (sub1 (PolyDots-n ty)))))
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
  (define row (parse-row row-stx))
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

