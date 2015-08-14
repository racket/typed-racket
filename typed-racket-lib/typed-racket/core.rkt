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
          (λ (new-mod pre-before-code pre-after-code)  ; `new-mod` is current `syntax-local-introduce`d
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
              [(after-code ...) (change-provide-fixups (flatten-all-begins pre-after-code))]
              ;; potentially optimize the code based on the type information
              [(optimized-body ...) (syntax-local-introduce
                                     (maybe-optimize ;; has own call to do-time
                                      #'transformed-body))]
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

(define did-I-suggest-:print-type-already? #f)
(define :print-type-message " ... [Use (:print-type <expr>) to see more.]")
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
     (tc-toplevel/full stx #'form
       (λ (body2 type) ; `body2` is current `syntax-local-introduce`d
         (with-syntax*
          ([(optimized-body ...) (maybe-optimize #`(#,body2))]
           ;; Transform after optimization for top-level because the flattening will
           ;; change syntax object identity (via syntax-track-origin) which doesn't work
           ;; for looking up types in the optimizer.
           [(transformed-body ...)
            (map
             syntax-local-introduce 
             (change-contract-fixups (flatten-all-begins #'(begin optimized-body ...))))])
          (define ty-str
            (match type
              ;; 'no-type means the form is not an expression and
              ;; has no meaningful type to print
              ['no-type #f]
              ;; don't print results of type void
              [(tc-result1: (== -Void type-equal?)) #f]
              ;; don't print results of unknown type
              [(tc-any-results: f) #f]
              [(tc-result1: t f o)
               ;; Don't display the whole types at the REPL. Some case-lambda types
               ;; are just too large to print.
               ;; Also, to avoid showing too precise types, we generalize types
               ;; before printing them.
               (define tc (cleanup-type t))
               (define tg (generalize tc))
               (format "- : ~a~a~a\n"
                       (pretty-format-type tg #:indent 4)
                       (cond [(equal? tc tg) ""]
                             [else (format " [more precisely: ~a]" tc)])
                       (cond [(equal? tc t) ""]
                             [did-I-suggest-:print-type-already? " ..."]
                             [else (set! did-I-suggest-:print-type-already? #t)
                                   :print-type-message]))]
              [(tc-results: t)
               (define tcs (map cleanup-type t))
               (define tgs (map generalize tcs))
               (define tgs-val (make-Values (map -result tgs)))
               (define formatted (pretty-format-type tgs-val #:indent 4))
               (define indented? (regexp-match? #rx"\n" formatted))
               (format "- : ~a~a~a\n"
                       formatted
                       (cond [(andmap equal? tgs tcs) ""]
                             [indented?
                              (format "\n[more precisely: ~a]"
                                      (pretty-format-type (make-Values (map -result tcs))
                                                          #:indent 17))]
                             [else (format " [more precisely: ~a]" (cons 'Values tcs))])
                       ;; did any get pruned?
                       (cond [(andmap equal? t tcs) ""]
                             [did-I-suggest-:print-type-already? " ..."]
                             [else (set! did-I-suggest-:print-type-already? #t)
                                   :print-type-message]))]
              [x (int-err "bad type result: ~a" x)]))
          (if (and ty-str
                   (not (null? (syntax-e #'(transformed-body ...)))))
              (with-syntax ([(transformed-body ... transformed-last)
                             #'(transformed-body ...)])
                #`(begin #,(if (unbox include-extra-requires?)
                               extra-requires
                               #'(begin))
                         #,(arm #'(begin transformed-body ...))
                         (begin0 #,(arm #'transformed-last)
                           (display '#,ty-str))))
              (arm #'(begin transformed-body ...))))))]))
