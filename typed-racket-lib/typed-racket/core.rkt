#lang racket/base

(require (rename-in "utils/utils.rkt")
         (for-syntax racket/base)
         (for-template racket/base)
         "private/with-types.rkt"
         "private/type-contract.rkt"
         (except-in syntax/parse id)
         racket/match racket/syntax
         syntax/flatten-begin
         "types/utils.rkt"
         "types/abbrev.rkt"
         "types/generalize.rkt"
         "typecheck/provide-handling.rkt"
         "typecheck/tc-app-helper.rkt"
         "rep/type-rep.rkt"
         (for-template "base-env/top-interaction.rkt")
         "utils/utils.rkt"
         "utils/tc-utils.rkt"
         "utils/arm.rkt"
         "standard-inits.rkt"
         "tc-setup.rkt")

(provide mb-core ti-core wt-core wt-core-shallow wt-core-optional)

(define-syntax-class type-enforcement-mode
  (pattern #:deep)
  (pattern #:shallow)
  (pattern #:optional))

(define (mb-core stx)
  (syntax-parse stx
    [(mb (~or (~optional te-mode:type-enforcement-mode)
              (~optional
               (~or (~and #:optimize    (~bind [opt? #'#t])); kept for backward compatibility
                    (~and #:no-optimize (~bind [opt? #'#f]))))
              (~optional
               (~and #:with-refinements refinement-reasoning?))
              (~optional
               (~and #:no-delay-errors no-delay-errors?)))
         ...
         forms ...)
     (let* ([pmb-form (syntax/loc stx (#%plain-module-begin forms ...))]
            [te-attr (attribute te-mode)]
            [te-mode (keyword->te-mode te-attr)]
            [delay-errors^? (or (not (attribute no-delay-errors?)) (delay-errors?))])
       (parameterize ([optimize? (or (and (not (attribute opt?)) (optimize?))
                                     (and (attribute opt?) (syntax-e (attribute opt?))))]
                      [with-refinements? (and (or (attribute refinement-reasoning?)
                                                  (with-refinements?))
                                              (unless (eq? te-mode deep)
                                                (raise-arguments-error
                                                 (string->symbol (format "typed/racket/~a"
                                                                         (keyword->string
                                                                          (syntax-e te-attr))))
                                                 "#:with-refinements unsupported")))])
         (tc-module/full te-mode stx pmb-form
          (λ (new-mod pre-before-code pre-after-code)
            (define ctc-cache (make-hash))
            (define (change-contract-fixups/cache forms)
              (change-contract-fixups forms ctc-cache))
            (define (change-provide-fixups/cache forms)
              (change-provide-fixups forms ctc-cache))
            (define (shallow-rewrite/cache body-stx)
              (define-values [extra-def* body+] (maybe-shallow-rewrite body-stx ctc-cache))
              (when extra-def*
                (set-box! include-extra-requires? #t))
              (cons (or extra-def* '()) body+))
            (with-syntax*
             (;; pmb = #%plain-module-begin
              [(pmb . body2) new-mod]
              ;; perform the provide transformation from [Culpepper 07]
              [transformed-body (begin0 (remove-provides #'body2) (do-time "Removed provides"))]
              [((before-rewritten-code ...) . rewritten-body) (shallow-rewrite/cache #'transformed-body)]
              ;; add the real definitions of contracts on requires
              [transformed-body
               (begin0 (change-contract-fixups/cache (syntax->list #'rewritten-body))
                       (do-time "Fixed contract ids"))]
              ;; add the real definitions of contracts on the before- and after-code
              [(before-code ...) (change-provide-fixups/cache (flatten-all-begins pre-before-code))]
              [(after-code ...) (begin0 (change-provide-fixups/cache (flatten-all-begins pre-after-code))
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
                     before-rewritten-code ... before-code ... optimized-body ... after-code ... check-syntax-help))))
          #:delay-errors? delay-errors^?)))]))

(define (ti-core stx)
  (current-type-names (init-current-type-names))
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ . (module . rest))
     #'(module . rest)]
    [(_ (~optional te:type-enforcement-mode) . (~and form ((~var command (static interactive-command? #f)) . _)))
     (do-standard-inits)
     ((interactive-command-procedure (attribute command.value)) #'form)]
    [(_ (~optional te:type-enforcement-mode) . form)
     (define te-mode (keyword->te-mode (attribute te)))
     ;; TODO(endobson): Remove the call to do-standard-inits when it is no longer necessary
     ;; Cast at the top-level still needs this for some reason
     (do-standard-inits)
     (tc-toplevel/full te-mode stx #'form)]))

;; Decide how to enforce types at runtime
(define (keyword->te-mode stx)
  (case (syntax-e (or stx #'#:deep))
    ((#:deep) deep)
    ((#:shallow) shallow)
    ((#:optional) optional)
    (else (error (format "Internal Typed Racket Error: unknown type enforcement mode ~s~n" stx)))))

