#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/list
         (for-syntax racket/base syntax/parse)
         (contract-req)
         (rep type-rep prop-rep object-rep rep-utils)
         (utils tc-utils)
         (types tc-result resolve subtype update union prop-ops)
         (env type-env-structs lexical-env mvar-env)
         (rename-in (types abbrev)
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (typecheck tc-metafunctions))

(provide with-lexical-env/extend-props)

;; Returns #f if anything becomes (U)
(define (env+ env ps)
  (define-values (props atoms) (combine-props ps (env-props env)))
  (cond
    [props
     (let loop ([ps atoms]
                [negs '()]
                [Γ (replace-props env props)])
       (match ps
         [(cons p ps)
          (match p
            [(TypeProp: (Path: lo x) pt)
             #:when (and (not (is-var-mutated? x))
                         (identifier-binding x))
             (let* ([t (lookup-type/lexical x Γ #:fail (lambda _ Univ))]
                    [new-t (update t pt #t lo)])
               (if (type-equal? new-t -Bottom)
                   (values #f '())
                   (loop ps negs (extend Γ x new-t))))]
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
                          (let* ([t (lookup-type/lexical x Γ #:fail (lambda _ Univ))]
                                 [new-t (update t pt #f lo)])
                            (if (type-equal? new-t -Bottom)
                                #f
                                (loop rst (extend Γ x new-t))))]
                         [_ Γ]))])
              (values Γ atoms))]))]
    [else (values #f '())]))

;; run code in an extended env and with replaced props. Requires the body to return a tc-results.
;; TODO make this only add the new prop instead of the entire environment once tc-id is fixed to
;; include the interesting props in its prop.
;; WARNING: this may bail out when code is unreachable
(define-syntax (with-lexical-env/extend-props stx)
  (define-splicing-syntax-class unreachable?
    (pattern (~seq #:unreachable form:expr))
    (pattern (~seq) #:with form #'(begin)))
  (syntax-parse stx
    [(_ ps:expr u:unreachable? . b)
     #'(let-values ([(new-env atoms) (env+ (lexical-env) ps)])
         (cond
           [new-env
            (with-lexical-env
             new-env
             (add-unconditional-prop (let () . b) (apply -and (append atoms (env-props new-env)))))]
           [else
            ;; unreachable, bail out
            u.form
            (ret -Bottom)]))]))
