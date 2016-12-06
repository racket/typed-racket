#lang racket/unit

;; This module provides typechecking for `send` method calls

(require "../utils/utils.rkt"
         racket/match syntax/stx racket/syntax
         syntax/parse
         (env lexical-env)
         (typecheck signatures tc-funapp tc-metafunctions)
         (types base-abbrev resolve utils type-table)
         (rep type-rep)
         (utils tc-utils hset)
         (for-template racket/base))

(import tc-expr^)
(export tc-send^)

(define (tc/send form app
                 rcvr-var rcvr
                 method-var method
                 arg-vars args
                 [expected #f])
  ;; do-check : Type? -> tc-results/c
  (define (do-check rcvr-type)
    (match rcvr-type
      [(Instance: (? resolvable? type))
       (do-check (make-Instance (resolve type)))]
      [(and obj (Instance: (Class: _ _ _ methods _ _)))
       (match (tc-expr/t method)
         [(Value: (? symbol? s))
          (define ftype
            (cond [(assq s methods) => cadr]
                  [else (tc-error/expr/fields
                         "send: method not understood by object"
                         "method name" s
                         "object type" obj
                         #:return -Bottom)]))
          (define vars  (list* rcvr-var method-var (syntax->list arg-vars)))
          (define types (list* rcvr-type ftype (stx-map tc-expr/t args)))
          (tc/send-internal vars types app expected)]
         [_ (int-err "non-symbol methods not supported by Typed Racket: ~a"
                     rcvr-type)])]
      ;; union of objects, check pointwise and union the results
      [(Union: objs) #:when (for/and ([t (in-hset objs)]) (Instance? t))
       (merge-tc-results (hset-map objs do-check))]
      [_ (tc-error/expr/fields
          "send: type mismatch"
          "expected" "an object"
          "given" rcvr-type)]))
  ;; Make sure to resolve before `do-check` because the type might
  ;; be a Name type or recursive type and thus unable to match Instance:
  (define final-ret (do-check (resolve (tc-expr/t rcvr))))
  (add-typeof-expr form final-ret)
  final-ret)

;; tc/send-internal : (Listof Id) (Listof Type) Syntax (Option TC-Result)
;;                    -> TC-Result
;; Handles typechecking the actual application inside the method send
;; expansion. Most of the work is done by tc/app via tc-expr.
(define (tc/send-internal vars types app-stx expected)
  (syntax-parse app-stx
    #:literal-sets (kernel-literals)
    #:literals (list)
    [(#%plain-app meth obj arg ...)
     (with-extended-lexical-env
       [#:identifiers vars
        #:types types]
       (tc-expr/check (syntax/loc app-stx (#%plain-app meth arg ...))
                      expected))]
    [(let-values ([(arg-var) arg] ...)
       (~and outer-loc (#%plain-app (~and inner-loc (#%plain-app cpce s-kp meth kpe kws num))
                                    kws2 kw-args
                                    obj pos-arg ...)))
     (with-extended-lexical-env
       [#:identifiers vars
        #:types types]
       (tc-expr/check
        (with-syntax* ([inner-app (syntax/loc app-stx (#%plain-app cpce s-kp meth kpe kws num))]
                       [outer-app (syntax/loc app-stx
                                    (#%plain-app inner-app kws2 kw-args pos-arg ...))])
          (syntax/loc app-stx (let-values ([(arg-var) arg] ...) outer-app)))
        expected))]))
