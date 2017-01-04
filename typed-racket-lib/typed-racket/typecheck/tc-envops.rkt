#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/list
         (for-syntax racket/base syntax/parse)
         (contract-req)
         (rep type-rep prop-rep object-rep rep-utils)
         (utils tc-utils)
         racket/set
         (types tc-result resolve subtype update prop-ops)
         (env type-env-structs lexical-env mvar-env)
         (rename-in (types abbrev)
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (typecheck tc-metafunctions))

(provide with-lexical-env+props
         impossible-in-lexical-env?)

(define (impossible-in-lexical-env? p)
  (not (env+ (lexical-env) (list p))))

;; Returns #f if anything becomes (U)
(define (env+ env ps)
  (cond
    [(null? ps) env]
    [else
     (define-values (props atoms) (combine-props ps (env-props env)))
     (cond
       [props
        (let loop ([ps atoms]
                   [negs '()]
                   [Γ (env-replace-props env props)])
          (match ps
            [(cons p ps)
             (match p
               [(TypeProp: (Path: lo x) pt)
                #:when (and (not (is-var-mutated? x))
                            (identifier-binding x))
                (let* ([t (lookup-type/lexical x Γ #:fail (λ (_) Univ))]
                       [new-t (update t pt #t lo)])
                  (and (not (Bottom? new-t))
                       (loop ps negs (env-set-type Γ x new-t))))]
               ;; process negative info _after_ positive info so we don't miss anything
               [(NotTypeProp: (Path: _ x) _)
                #:when (and (not (is-var-mutated? x))
                            (identifier-binding x))
                (loop ps (cons p negs) Γ)]
               [_ (loop ps negs Γ)])]
            [_ (let ([Γ (let loop ([negs negs]
                                   [Γ Γ])
                          (match negs
                            [(cons (NotTypeProp: (Path: lo x) pt) rst)
                             (let* ([t (lookup-type/lexical x Γ #:fail (λ (_) Univ))]
                                    [new-t (update t pt #f lo)])
                               (and (not (Bottom? new-t))
                                    (loop rst (env-set-type Γ x new-t))))]
                            [_ Γ]))])
                 (and Γ (env-replace-props Γ (append atoms (env-props Γ)))))]))]
       [else #f])]))


;; run code in an extended env and with replaced props.
;; Requires the body to return a tc-results.
;; WARNING: this may bail out when code is unreachable
(define-syntax (with-lexical-env+props stx)
  (define-splicing-syntax-class unreachable?
    (pattern (~seq #:unreachable form:expr))
    (pattern (~seq) #:with form #'(begin)))
  (syntax-parse stx
    [(_ ps:expr
        #:expected expected
        u:unreachable? . b)
     (syntax/loc stx
       (let ([old-props (env-props (lexical-env))]
             [new-env (env+ (lexical-env) ps)])
         (cond
           [new-env
            (with-lexical-env
              new-env
              (let ([result (let () . b)])
                (match expected
                  ;; if there was not any expected results, then
                  ;; return any new info that was learned while
                  ;; extending the environment
                  [(or #f (tc-any-results: #f))
                   (define new-props
                     (make-AndProp (set-subtract (env-props new-env) old-props)))
                   (add-unconditional-prop result new-props)]
                  ;; otherwise, just return the expected results
                  [_ (fix-results expected)])))]
           [else
            ;; unreachable, bail out
            u.form
            (ret -Bottom)])))]))
