#lang racket/base

(require (for-syntax racket/base racket/lazy-require))

(begin-for-syntax
  (lazy-require [(submod "." implementation)
                 (:type-impl :kind-impl :print-type-impl :query-type/args-impl :query-type/result-impl)]))

(provide
  (for-syntax
    interactive-command?
    interactive-command-procedure)
  :type :print-type :query-type/args :query-type/result :kind)

(define-for-syntax (fail _ stx)
  (syntax-case stx ()
    [_
     (identifier? stx)
     (raise-syntax-error #f "must be applied to arguments" stx)]
    [_ (raise-syntax-error #f "only valid at the top-level of an interaction" stx)]))


(begin-for-syntax
  (struct interactive-command (procedure)
    #:property prop:procedure fail))


(define-syntax :type (interactive-command :type-impl))
(define-syntax :kind (interactive-command :kind-impl))
(define-syntax :print-type (interactive-command :print-type-impl))
(define-syntax :query-type/args (interactive-command :query-type/args-impl))
(define-syntax :query-type/result (interactive-command :query-type/result-impl))

(module implementation racket/base
  (require
    "../utils/utils.rkt"
    racket/base
    racket/match
    racket/format
    racket/string
    racket/list
    syntax/stx
    syntax/parse
    "../tc-setup.rkt"
    "../private/parse-type.rkt"
    "../private/syntax-properties.rkt"
    "../types/utils.rkt"
    "../types/abbrev.rkt"
    "../types/printer.rkt"
    "../types/resolve.rkt"
    "../typecheck/possible-domains.rkt"
    "../typecheck/typechecker.rkt"
    "../rep/type-rep.rkt"
    "../rep/type-constr.rkt"
    "../utils/tc-utils.rkt"
    (for-syntax racket/base syntax/parse)
    (for-template racket/base))
  (provide
    :type-impl :print-type-impl :query-type/args-impl :query-type/result-impl :kind-impl)

  (define (with-type-printing verbose? type-form? thunk)
    (parameterize ([current-print-type-fuel
                    (cond
                      [verbose? +inf.0]
                      [type-form? 1]
                      [else (current-print-type-fuel)])]
                   ;; This makes sure unions are totally flat for the
                   ;; infinite fuel case. If fuel that's not 0, 1, or +inf.0
                   ;; is ever used, more may need to be done.
                   [current-type-names
                    (if verbose? '() (current-type-names))]
                   [current-print-unexpanded (box '())]
                   [print-complex-props? (or verbose? (print-complex-props?))])
      (thunk)))

  ;; this one doesn't quite fit the pattern of the next three REPL operations, so
  ;; this one isn't defined with a macro as below
  (define (:type-impl stx)
    (syntax-parse stx
      [(_ (~optional (~and #:verbose verbose-kw)) ty:expr)
       (with-type-printing (attribute verbose-kw) #t
        (λ ()
         (define type (pretty-format-rep (match (parse-type #'ty)
                                           [(? App? ty) (resolve ty)]
                                           [ty ty])))
         (define unexpanded
           (remove-duplicates (unbox (current-print-unexpanded))))
         (define cue (if (null? unexpanded)
                         ""
                         (format "[can expand further: ~a]"
                                 (string-join (map ~a unexpanded)))))
         #`(display #,(format "~a\n~a" type cue))))]
      [form
       (raise-syntax-error #f "must be applied to exactly one argument" #'form)]))

  (define (:kind-impl stx)
    (syntax-parse stx
      [(_ k:expr)
       (define kind (parse-type-or-type-constructor #'k))
       (define kind-msg (print-kind kind))
       #`(display #,kind-msg)]))

  (define-syntax (define-repl-op stx)
    (syntax-parse stx
      [(_ op args to-expand handler err)
       #'(define (op stx)
           (syntax-parse stx
             [args
              (define result
                (tc-expr (local-expand to-expand 'expression (list #'module*))))
              (handler result)]
             [form
              (raise-syntax-error #f err #'form)]))]))

  (define (:print-type-impl stx)
    (syntax-parse stx
      [(_ (~optional (~and #:verbose verbose-kw)) e:expr)
       (define result
         (tc-expr (local-expand #'e 'expression (list #'module*))))
       (with-type-printing (attribute verbose-kw) #f
        (λ ()
          #`(displayln
             #,(pretty-format-rep
                (match result
                  [(tc-result1: t f o) t]
                  [(tc-results: (list (tc-result: ts) ...) _)
                   (-values ts)]
                  [(tc-any-results: f) (-AnyValues f)])))))]
      [form
       (raise-syntax-error #f "must be applied to exactly one argument" #'form)]))

  ;; given a function and input types, display the result type
  (define-repl-op :query-type/args-impl (_ op arg-type ...)
    (with-syntax ([(dummy-arg ...) (generate-temporaries #'(arg-type ...))])
      ;; create a dummy function with the right argument types
      #`(lambda #,(stx-map type-label-property
                           #'(dummy-arg ...) #'(arg-type ...))
                (op dummy-arg ...)))
     (λ (type)
       #`(display
          #,(pretty-format-rep
             (match type
               [(tc-result1: (and t (? Fun?)) f o) t]))))
     "must be applied to at least one argument" )

  ;; given a function and a desired return type, fill in the blanks
  (define-repl-op :query-type/result-impl (_ op desired-type) #'op
    (λ (type)
      (match type
        [(tc-result1: (and t (? Fun?)) f o)
         (let ([cleaned (cleanup-type t (parse-type #'desired-type) #f)])
           #`(display
              #,(match cleaned
                  [(Fun: '())
                   "Desired return type not in the given function's range.\n"]
                  [(Fun: _)
                   (pretty-format-rep cleaned)])))]
        [_ (error (format "~a: not a function" (syntax->datum #'op)))]))
    "must be applied to exactly two arguments"))
