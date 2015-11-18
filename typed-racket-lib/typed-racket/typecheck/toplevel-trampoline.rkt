#lang racket/base

;; This module implements Typed Racket's trampolining top-level
;; typechecking. The entrypoint is the function provided below, which
;; sets up the trampoline.
;;
;; Subsequently, the macros forms defined in the submodule at the bottom
;; take over and keep head local-expanding until `begin` forms are exhausted,
;; at which point the syntax can be fully-expanded and checked normally.

(require "../utils/utils.rkt"
         syntax/parse
         (private syntax-properties)
         (for-template racket/base))

(provide tc-toplevel-start)

;; entrypoint for typechecking a top-level interaction
;; this is defined in this module (instead of tc-top-level.rkt) in
;; order to avoid cyclic dependency issues
;; syntax syntax -> syntax
(define (tc-toplevel-start orig-stx stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    ;; Don't open up `begin`s that are supposed to be ignored
    [(~and (begin e ... e-last)
           (~not (~or _:ignore^ _:ignore-some^)))
     ;; the original syntax is threaded through for error message reporting
     ;; later in `trampoline-core`
     #`(begin (tc-toplevel-trampoline (quote-syntax #,orig-stx) e) ...
              (tc-toplevel-trampoline/report
               (quote-syntax #,orig-stx) e-last))]))

(module trampolines racket/base
  (require "../utils/utils.rkt"
           (for-syntax racket/base
                       racket/match
                       syntax/kerncase
                       syntax/parse
                       syntax/stx
                       (rep type-rep)
                       (optimizer optimizer)
                       (types utils abbrev printer generalize)
                       (typecheck tc-toplevel tc-app-helper)
                       (private type-contract syntax-properties)
                       (env mvar-env)
                       (utils disarm lift utils timing tc-utils arm mutated-vars)))

  (provide tc-toplevel-trampoline
           tc-toplevel-trampoline/report)

  (define-for-syntax (maybe-optimize body)
    ;; do we optimize?
    (if (optimize?)
        (begin
          (do-time "Starting optimizer")
          (begin0 (stx-map optimize-top body)
            (do-time "Optimized")))
        body))

  (define-for-syntax (trampoline-core stx report? kont)
    (syntax-parse stx
      [(_ orig-stx e)
       (define head-expanded
         (disarm*
          (local-expand/capture* #'e 'top-level (kernel-form-identifier-list))))
       (syntax-parse head-expanded
         #:literal-sets (kernel-literals)
         [(begin (define-values (n) _) ...
                 (~and (~or _:ignore^ _:ignore-some^)
                       (~not (~or _:tr:class^
                                  _:tr:unit^
                                  _:tr:unit:invoke^
                                  _:tr:unit:compound^
                                  _:tr:unit:from-context^))))
          head-expanded]
         ;; keep trampolining on begins
         [(begin (define-values (n) e-rhs) ... (begin e ... e-last))
          #`(begin (tc-toplevel-trampoline orig-stx (define-values (n) e-rhs))
                   ...
                   (tc-toplevel-trampoline orig-stx e) ...
                   #,(if report?
                         #'(tc-toplevel-trampoline/report orig-stx e-last)
                         #'(tc-toplevel-trampoline orig-stx e-last)))]
         [_
          (define fully-expanded
            ;; a non-begin form can still cause lifts, so still have to catch them
            (disarm* (local-expand/capture* #'e 'top-level (list #'module*))))
          (find-mutated-vars fully-expanded mvar-env)
          ;; Unlike the `begin` cases, we probably don't need to trampoline back
          ;; to the top-level because we're not catching lifts from macros at the
          ;; top-level context but instead from expression context.
          (parameterize ([orig-module-stx #'orig-stx]
                         [expanded-module-stx fully-expanded])
            (syntax-parse fully-expanded
              #:literal-sets (kernel-literals)
              [(begin form ...)
               (define forms (syntax->list #'(form ...)))
               (define result
                 (for/last ([form (in-list forms)])
                   (tc-toplevel-form form)))
               ;; Transform after optimization for top-level because the flattening
               ;; will change syntax object identity (via syntax-track-origin) which
               ;; doesn't work for looking up types in the optimizer.
               (define new-stx
                 (apply append
                        (for/list ([form (in-list forms)])
                          (change-contract-fixups (maybe-optimize (list form))))))
               (kont new-stx result)]))])]))

  ;; Trampoline that continues the typechecking process.
  (define-syntax (tc-toplevel-trampoline stx)
    (trampoline-core
     stx #f
     (λ (new-stx result)
       (arm
        (if (unbox include-extra-requires?)
            #`(begin #,extra-requires #,@new-stx)
            #`(begin #,@new-stx))))))

  (begin-for-syntax
    (define did-I-suggest-:print-type-already? #f)
    (define :print-type-message " ... [Use (:print-type <expr>) to see more.]"))

  ;; Trampoline that continues the typechecking process and reports the type
  ;; information to the user.
  (define-syntax (tc-toplevel-trampoline/report stx)
    (trampoline-core
     stx #t
     (λ (new-stx result)
       (define ty-str
         (match result
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
       (define with-printing
        (with-syntax ([(e ... e-last) new-stx])
          (if ty-str
              #`(begin e ...
                       (begin0 e-last (display '#,ty-str)))
              #'(begin e ... e-last))))
       (arm
        (if (unbox include-extra-requires?)
            #`(begin #,extra-requires #,with-printing)
            with-printing))))))

(require (for-template (submod "." trampolines)))
