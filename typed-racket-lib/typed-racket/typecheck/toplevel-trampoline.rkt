#lang racket/base

;; This module implements Typed Racket's trampolining top-level
;; typechecking. The entrypoint is the function provided below, which
;; sets up the trampoline.
;;
;; Subsequently, the macros forms defined in the submodule at the bottom
;; take over and keep head local-expanding until `begin` forms are exhausted,
;; at which point the syntax can be fully-expanded and checked normally.

(require syntax/parse
         "../private/syntax-properties.rkt"
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
  (require (for-syntax racket/base
                       racket/match
                       syntax/kerncase
                       syntax/parse
                       syntax/stx
                       "../rep/values-rep.rkt"
                       "../optimizer/optimizer.rkt"
                       "../types/utils.rkt"
                       "../types/abbrev.rkt"
                       "../types/printer.rkt"
                       "../types/tc-result.rkt"
                       "tc-toplevel.rkt"
                       "../private/type-contract.rkt"
                       "../private/syntax-properties.rkt"
                       (only-in "../types/subtype.rkt" subtype)
                       "../env/mvar-env.rkt"
                       "../utils/disarm.rkt"
                       "../utils/lift.rkt"
                       "../utils/utils.rkt"
                       "../utils/timing.rkt"
                       "../utils/tc-utils.rkt"
                       "../utils/arm.rkt"
                       "../utils/mutated-vars.rkt"))

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
         ;; keep trampolining on begins, transfer syntax properties so that ignore
         ;; properties are retained in the begin subforms
         [(begin (define-values (n) e-rhs) ...
                 (~and the-begin (begin e ... e-last)))
          (define e*s
            (for/list ([e (in-list (syntax->list #'(e ...)))])
              (syntax-track-origin e #'the-begin #'begin)))
          (define e-last*
            (syntax-track-origin #'e-last #'the-begin #'begin))
          (with-syntax ([(e ...) e*s]
                        [e-last e-last*])
            #`(begin (tc-toplevel-trampoline orig-stx (define-values (n) e-rhs))
                     ...
                     (tc-toplevel-trampoline orig-stx e) ...
                     #,(if report?
                           #'(tc-toplevel-trampoline/report orig-stx e-last)
                           #'(tc-toplevel-trampoline orig-stx e-last))))]
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
           [(? tcresult-at-toplevel?) (toplevel-report result)]
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
