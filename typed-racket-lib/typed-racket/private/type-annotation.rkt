#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep)
         (utils tc-utils)
         (env global-env mvar-env)
         (except-in (types subtype abbrev utils generalize)
                    -> ->* one-of/c)
         (private parse-type syntax-properties)
         (contract-req)
         syntax/parse
         racket/match)

(provide type-annotation
         type-annotation^
         get-type
         get-types
         get-type/infer
         check-type
         dotted?)


;; get the type annotation of this syntax
;; syntax #:infer boolean? #:lookup? boolean? -> Maybe[Type]
;; is let-binding really necessary? - remember to record the bugs!
;; if lookup? is #f, do not search the global env for a type (fix GH issue #849)
(define (type-annotation stx #:infer [let-binding #f] #:lookup? [lookup? #true])
  (define (pt prop)
    (when (and (identifier? stx)
               let-binding)
      (define t1 (parse-type/id stx prop))
      (define t2 (lookup-type stx (lambda () #f)))
      (when (and t2 (not (equal? t1 t2)))
        (maybe-finish-register-type stx)
        (tc-error/delayed #:stx stx "Duplicate type annotation of ~a for ~a, previous was ~a" t1 (syntax-e stx) t2)))
    (if (syntax? prop)
        (parse-type prop)
        (parse-type/id stx prop)))
  (syntax-parse stx
    [v:type-label^
     (pt (attribute v.value))]
    [_:id
     #:when lookup?
     (define ty (lookup-type stx #f))
     (when ty
       (maybe-finish-register-type stx))
     ty]
    [_ #f]))

(define-syntax-class type-annotation^
  [pattern i:id
           #:attr type (type-annotation #'i)
           #:when (attribute type)])

;; get the type annotation of this identifier, otherwise error
;; if #:default is provided, return that instead of error
;; identifier #:default Type -> Type
(define (get-type stx #:default [default #f] #:infer [infer #t])
  (parameterize
      ([current-orig-stx stx])
    (cond
      [(type-annotation stx #:infer infer)]
      [(procedure? default) (default)]
      [default default]
      [(not (syntax-original? stx))
       (tc-error "insufficient type information to typecheck. please add more type annotations")]
      [else
       (tc-error "no type information on variable ~a" (syntax-e stx))])))

;; Listof[identifier] #:default Type -> Listof[Type]
(define (get-types stxs #:default [default #f])
  (map (lambda (e) (get-type e #:default default)) stxs))

;; stxs : the identifiers, possibly with type annotations on them
;; expr : the RHS expression
;; tc-expr : a function like `tc-expr' from tc-expr-unit
;; tc-expr/check : a function like `tc-expr/check' from tc-expr-unit
(define/cond-contract (get-type/infer stxs expr tc-expr tc-expr/check)
  ((listof identifier?)
   syntax?
   (syntax? . -> . tc-results/c)
   (syntax? tc-results/c . -> . tc-results/c)
   . -> .
   (listof tc-result?))
  (match stxs
    [(list stx ...)
     (let ([anns (for/list ([s (in-list stxs)])
                   (type-annotation s #:infer #t))])
       (if (for/and ([a (in-list anns)]) a)
           (match (tc-expr/check expr (ret anns))
             [(tc-results: tcrs _) tcrs])
           (match (tc-expr expr)
             [(tc-any-results: _)
              (tc-error/expr
               #:return (map (λ _ (-tc-result -Bottom)) stxs)
               "Expression should produce ~a values, but produces an unknown number of values"
               (length stxs))]
             [(tc-result1: (== -Bottom))
              (for/list ([_ (in-range (length stxs))])
                (-tc-result -Bottom))]
             [(tc-results: tcrs _)
              (cond
                [(not (= (length stxs) (length tcrs)))
                 (tc-error/expr #:return (map (λ _ (-tc-result -Bottom)) stxs)
                                "Expression should produce ~a values, but produces ~a values of types ~a"
                                (length stxs)
                                (length tcrs)
                                (stringify (map tc-result-t tcrs)))]
                [else
                 (for/list ([stx (in-list stxs)]
                            [tcr (in-list tcrs)]
                            [a   (in-list anns)])
                   (match tcr
                     [(tc-result: ty ps o)
                      (cond [a (check-type stx ty a)
                               (-tc-result a ps o)]
                            ;; mutated variables get generalized, so that we don't
                            ;; infer too small a type
                            [(is-var-mutated? stx)
                             (-tc-result (generalize ty) ps o)]
                            [else (-tc-result ty ps o)])]))])])))]))

;; check that e-type is compatible with ty in context of stx
;; otherwise, error
;; syntax type type -> void
(define (check-type stx e-type ty)
  (parameterize ([current-orig-stx stx])
    (unless (subtype e-type ty)
      ;(printf "orig-stx: ~a" (syntax->datum stx*))
      (tc-error "Body had type:\n~a\nVariable had type:\n~a\n" e-type ty))))

(define (dotted? stx)
  (syntax-parse stx
    [v:type-dotted^ (syntax-e (attribute v.value))]
    [_ #f]))
