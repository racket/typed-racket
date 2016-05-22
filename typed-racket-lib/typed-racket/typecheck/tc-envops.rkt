#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/list
         (for-syntax racket/base syntax/parse)
         (contract-req)
         (rep type-rep prop-rep object-rep rep-utils)
         (utils tc-utils)
         (types tc-result resolve subtype remove update union prop-ops)
         (env type-env-structs lexical-env)
         (rename-in (types abbrev)
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (typecheck tc-metafunctions))

(provide with-lexical-env/extend-props)

;; Returns #f if anything becomes (U)
(define (env+ env ps)
  (let/ec exit*
    (define (exit) (exit* #f empty))
    (define-values (props atoms) (combine-props ps (env-props env) exit))
    (values
      (for/fold ([Γ (replace-props env props)]) ([p (in-list atoms)])
        (match p
          [(or (TypeProp: (Path: lo x) pt) (NotTypeProp: (Path: lo x) pt))
           (update-type/lexical
             (lambda (x t)
               (define new-t (update t pt (TypeProp? p) lo))
               (when (type-equal? new-t -Bottom)
                 (exit))
               new-t)
             x Γ)]
          [_ Γ]))
      atoms)))

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
         (if new-env
             (with-lexical-env new-env
               (add-unconditional-prop (let () . b) (apply -and (append atoms (env-props new-env)))))
             ;; unreachable, bail out
             (let ()
               u.form
               (ret -Bottom))))]))
