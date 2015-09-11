#lang racket/base

(require (rename-in "utils/utils.rkt")
         (for-syntax racket/base)
         (for-template racket/base)
         (private with-types type-contract)
         (except-in syntax/parse id)
         racket/match racket/syntax
         syntax/flatten-begin
         (types utils abbrev generalize)
         (typecheck provide-handling tc-app-helper)
         (rep type-rep)
         (for-template (base-env top-interaction))
         (utils utils tc-utils arm)
         (only-in (types printer) pretty-format-type)
         "standard-inits.rkt"
         "tc-setup.rkt")

(provide mb-core ti-core wt-core)

(define (mb-core stx)
  (syntax-parse stx
    [(mb (~optional (~or (~and #:optimize    (~bind [opt? #'#t])) ; kept for backward compatibility
                         (~and #:no-optimize (~bind [opt? #'#f]))))
         forms ...)
     (let ([pmb-form (syntax/loc stx (#%plain-module-begin forms ...))])
       (parameterize ([optimize? (or (and (not (attribute opt?)) (optimize?))
                                     (and (attribute opt?) (syntax-e (attribute opt?))))])
         (tc-module/full stx pmb-form
          (λ (new-mod pre-before-code pre-after-code)
            (with-syntax*
             (;; pmb = #%plain-module-begin
              [(pmb . body2) new-mod]
              ;; perform the provide transformation from [Culpepper 07]
              [transformed-body (begin0 (remove-provides #'body2) (do-time "Removed provides"))]
              ;; add the real definitions of contracts on requires
              [transformed-body
               (begin0 (change-contract-fixups (syntax->list #'transformed-body))
                       (do-time "Fixed contract ids"))]
              ;; add the real definitions of contracts on the before- and after-code
              [(before-code ...) (change-provide-fixups (flatten-all-begins pre-before-code))]
              [(after-code ...) (begin0 (change-provide-fixups (flatten-all-begins pre-after-code))
                                  (do-time "Generated contracts"))]
              ;; potentially optimize the code based on the type information
              [(optimized-body ...) (maybe-optimize #'transformed-body)] ;; has own call to do-time
              ;; add in syntax property on useless expression to draw check-syntax arrows
              [check-syntax-help (syntax-property
                                  (syntax-property
                                   #'(void)
                                   'disappeared-binding (disappeared-bindings-todo))
                                  'disappeared-use (disappeared-use-todo))])
             ;; reconstruct the module with the extra code
             ;; use the regular %#module-begin from `racket/base' for top-level printing
             (arm #`(#%module-begin 
                     #,(if (unbox include-extra-requires?) extra-requires #'(begin))
                     before-code ... optimized-body ... after-code ... check-syntax-help)))))))]))

(define (ti-core stx )
  (current-type-names (init-current-type-names))
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ . (module . rest))
     #'(module . rest)]
    [(_ . (~and form ((~var command (static interactive-command? #f)) . _)))
     (do-standard-inits)
     ((interactive-command-procedure (attribute command.value)) #'form)]
    [(_ . form)
     ;; TODO(endobson): Remove the call to do-standard-inits when it is no longer necessary
     ;; Cast at the top-level still needs this for some reason
     (do-standard-inits)
     (tc-toplevel/full stx #'form)]))
