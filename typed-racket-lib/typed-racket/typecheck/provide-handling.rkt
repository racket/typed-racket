#lang racket/base

(require "../utils/utils.rkt"
         unstable/list unstable/sequence syntax/id-table racket/dict racket/syntax
         racket/struct-info racket/match syntax/parse
         (only-in (private type-contract) include-extra-requires?)
         (private syntax-properties)
         (typecheck renamer def-binding)
         (utils tc-utils)
         (for-syntax racket/base)
         (for-template racket/base "def-export.rkt"))

(provide remove-provides provide? generate-prov get-alternate)

(define (provide? form)
  (syntax-parse form
    #:literal-sets (kernel-literals)
    [(#%provide . rest) form]
    [_ #f]))

(define (remove-provides forms)
  (for/list ([e (in-syntax forms)]
             #:unless (provide? e))
    e))

(define (freshen-id id)
  ((make-syntax-introducer) id))

;; generate-prov : dict[id -> def-binding] dict[id -> list[id]] id id
;;                 -> (values listof[syntax] listof[listof[list[id id]]])
;; defs: defines in this module
;; provs: provides in this module
;; pos-blame-id: a #%variable-reference for the module
;; mk-redirect-id: the name of a definition created by `make-make-redirect-to-contract`

;; The first returned value is a syntax object of definitions that defines the
;; contracted versions of the provided identifiers, and the corresponding
;; provides.
;;
;; The second value is a list of two element lists, which are type name aliases.
(define (generate-prov defs provs pos-blame-id mk-redirect-id)
  ;; maps ids defined in this module to an identifier which is the possibly-contracted version of the key
  (define mapping (make-free-id-table))

  ;; quad/c in the signatures corresponds to four values:
  ;; (values syntax? syntax? identfier? (listof (list/c identifier? identifier?))
  ;; First return value is a syntax object of definitions, which will go in
  ;;    the #%contract-defs submodule
  ;; Second is a syntax object of definitions to go in the main module, including 
  ;;    the defintion to be exported
  ;; Third is the id to export
  ;; Fourth is a list of two element lists representing type aliases


  ;; mk-ignored-quad : identifier -> quad/c
  (define (mk-ignored-quad i) (values #'(begin) #'(begin) i null))

  ;; mk : id -> quad/c
  ;;
  ;; internal-id : the id being provided. If `internal-id' is defined
  ;; in this module, we will produce a (begin def ... provide) block
  ;; and a name to provide instead of internal-id.
  ;;
  ;; Anything already recorded in the mapping is given an empty
  ;; (begin) and the already-recorded id otherwise, we will map
  ;; internal-id to the fresh id in `mapping'
  (define (mk internal-id)
    (define new-id (freshen-id internal-id))
    (cond
      ;; if it's already done, do nothing
      [(dict-ref mapping internal-id
                 ;; if it wasn't there, put it in, and skip this case
                 (λ () (dict-set! mapping internal-id new-id) #f))
       => mk-ignored-quad]
      [(dict-ref defs internal-id #f)
       =>
       (match-lambda
         [(def-binding _ ty)
          (mk-value-quad internal-id new-id ty)]
         [(def-struct-stx-binding _ (? struct-info? si) constr-type)
          (mk-struct-syntax-quad internal-id new-id si constr-type)]
         [(def-stx-binding _)
          (mk-syntax-quad internal-id new-id)])]
      ;; otherwise, not defined in this module, not our problem
      [else (mk-ignored-quad internal-id)]))

  ;; mk-struct-syntax-quad : identifier? identifier? struct-info? Type/c -> quad/c
  ;; This handles `(provide s)` where `s` was defined with `(struct s ...)`. 
  (define (mk-struct-syntax-quad internal-id new-id si constr-type)
    (define type-is-constructor? #t) ;Conservative estimate (provide/contract does the same)
    (match-define (list type-desc constr pred (list accs ...) muts super) (extract-struct-info si))
    (define-values (defns export-defns new-ids aliases)
      (map/values 4
                  (lambda (e) (if (identifier? e)
                                  (mk e)
                                  (mk-ignored-quad e)))
                  (list* type-desc pred super accs)))
    ;; Here, we recursively handle all of the identifiers referenced
    ;; in this static struct info.
    (define-values (constr-defn constr-export-defn constr-new-id constr-aliases)
      (cond
       [(not (identifier? constr))
        (values #'(begin) #'(begin) #f null)]
       [(free-identifier=? constr internal-id)
        (mk-value-quad constr (generate-temporary constr) constr-type)]
       [else
        (mk constr)]))

    (define/with-syntax (constr* type-desc* pred* super* accs* ...)
      (for/list ([i (in-list (cons constr-new-id new-ids))])
        (and (identifier? i) #`(quote-syntax #,i))))

    (with-syntax* ([id internal-id]
                   [export-id new-id]
                   [protected-id (freshen-id #'id)])
      (values
        #`(begin
            #,constr-defn
            #,@defns)
        #`(begin
            #,constr-export-defn
            #,@export-defns
            ;; Here, we construct a new static struct identifier whose
            ;; contents point to newly-defined identifiers that are
            ;; themselves redirections. Unlike for value exports, we
            ;; don't provide two distinct identifiers, one for typed
            ;; code and one for untyped code, because they both have
            ;; to accessible by `syntax-local-value` and thus have to
            ;; be protected from re-export regardless of whether the
            ;; identifiers are copied out. Additionally, we can't put
            ;; a protected version in the submodule, since that
            ;; wouldn't be accessible by `syntax-local-value`.
            (define-syntax protected-id
              (let ((info (list type-desc* (syntax export-id) pred* (list accs* ...)
                                (list #,@(map (lambda (x) #'#f) accs)) super*)))
                #,(if type-is-constructor?
                      #'(make-struct-info-self-ctor constr* info)
                      #'info)))
            (def-export export-id protected-id protected-id))
        #'export-id
        (cons (list #'export-id internal-id)
              (apply append constr-aliases aliases)))))


  ;; mk-syntax-quad : identifier? identifier? -> quad/c
  (define (mk-syntax-quad internal-id new-id)
    (with-syntax* ([id internal-id]
                   [export-id new-id]
                   [untyped-id (freshen-id #'id)])
      (values
       #`(begin)
       ;; There's no need to put this macro in the submodule since it
       ;; has no dependencies.
       #`(begin 
           (define-syntax (untyped-id stx)
             (tc-error/stx stx "Macro ~a from typed module used in untyped code" 'untyped-id))
           (def-export export-id id untyped-id))
       new-id
       (list (list #'export-id #'id)))))

  ;; mk-value-quad : identifier? identifier? (or/c Type #f) -> quad/c
  (define (mk-value-quad internal-id new-id ty)
    (with-syntax* ([id internal-id]
                   [untyped-id (freshen-id #'id)]
                   [local-untyped-id (freshen-id #'id)]
                   [export-id new-id])
      (define/with-syntax ctc (generate-temporary 'generated-contract))
      ;; Create the definitions of the contract and the contracted export.
      (define/with-syntax definitions
        (contract-def/provide-property
         #'(define-values (ctc) #f)
         (list ty #'untyped-id #'id pos-blame-id)))
      (values
       ;; For the submodule
       #`(begin definitions (provide untyped-id))
       ;; For the main module
       #`(begin (define-syntax local-untyped-id (#,mk-redirect-id (quote-syntax untyped-id)))
                (def-export export-id id local-untyped-id))
       new-id
       null)))


  ;; Build the final provide with auxilliary definitions
  (for/lists (defs export-defs provides aliases) ([(internal-id external-ids) (in-dict provs)])
    (define-values (defs export-def id alias) (mk internal-id))
    (define provide-forms
      (for/list ([external-id (in-list external-ids)])
        #`(rename-out [#,id #,external-id])))
    (values #`(begin #,defs)
            export-def
            #`(provide #,@provide-forms)
            alias)))
