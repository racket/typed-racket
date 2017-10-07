#lang racket/unit

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         racket/match racket/list
         (typecheck signatures tc-app-helper)
         (types utils abbrev substitute type-table)
         (utils tc-utils)
         (rep type-rep core-rep values-rep)
         (r:infer infer))

(import tc-expr^ tc-lambda^ tc-let^ tc-app^)
(export tc-apply^)

(define (do-ret t)
  (match t
    [(Values: (list (Result: ts _ _) ...)) (ret ts)]
    [(ValuesDots: (list (Result: ts _ _) ...) dty dbound)
     (ret ts
          (for/list ([t (in-list ts)]) -tt-propset)
          (for/list ([t (in-list ts)]) -empty-obj)
          dty dbound)]
    [_ (int-err "do-ret fails: ~a" t)]))

(define (tc/apply f args)
  (define f-ty (single-value f))
  ;; produces the first n-1 elements of the list, and the last element
  (define (split l) (let-values ([(f r) (split-at l (sub1 (length l)))])
                      (values f (car r))))
  (define-values (fixed-args tail)
    (let ([args* (syntax->list args)])
      (if (null? args*)
          (tc-error "apply requires a final list argument, given only a function argument of type ~a" (match f-ty [(tc-result1: t) t]))
          (split args*))))

  (define arg-tres (map tc-expr fixed-args))
  (define arg-tys (map (match-lambda [(tc-result1: t _ _) t]) arg-tres))
  (define full-tail-ty (tc-expr/t tail))
  (define-values (tail-ty tail-bound)
    (match full-tail-ty
      [(ListDots: tail-ty tail-bound)
       (values tail-ty tail-bound)]
      [t (values #f #f)]))

  ;; Raises an error message for the case that the arguments do not match any of the domains
  (define (failure)
    (match f-ty
      [(tc-result1:
         (and t (AnyPoly-names: _ _
                                (Fun: (list (Arrow: doms rests (list (Keyword: _ _ #f) ...) rngs) ..1)))))
       (domain-mismatches f args t doms rests rngs arg-tres full-tail-ty #f
                          #:msg-thunk (lambda (dom)
                                        (string-append
                                         "Bad arguments to function in `apply':\n"
                                         dom)))]))

  (match f-ty
    ;; apply of a simple function or polymorphic function
    [(tc-result1:
      (AnyPoly: vars dotted-vars (Fun: (list arrows ..1))))
     #:when (not (for*/or ([a (in-list arrows)]
                           [kws (in-value (Arrow-kws a))])
                   (ormap Keyword-required? kws)))
     (or
      (for/or ([arrow (in-list arrows)])
        (match arrow
          [(Arrow: domain rst _ rng)
           ;; Takes a possible substitution and computes
           ;; the substituted range type if it is not #f
           (define (finish substitution)
             (begin0
               (and substitution (do-ret (subst-all substitution rng)))
               (add-typeof-expr f (ret (make-Fun (list arrow))))))
           (finish
            (infer vars dotted-vars
                   (list (-Tuple* arg-tys full-tail-ty))
                   (list (-Tuple* domain (Rest->Type rst)))
                   rng))]))
       (failure))]
    [(tc-result1: (AnyPoly: _ _ (Fun: '())))
     (tc-error/expr "Function has no cases")]
    [(tc-result1: f-ty)
     (tc-error/expr "Type of argument to apply is not a function type: \n~a" f-ty)]))
