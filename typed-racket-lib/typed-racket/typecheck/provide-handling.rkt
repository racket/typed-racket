#lang racket/base

(require "../utils/utils.rkt"
         racket/sequence
         syntax/parse
         (private syntax-properties)
         (typecheck def-binding)
         (env env-utils)
         (for-syntax racket/base)
         (for-template racket/base))

(provide remove-provides provide? generate-prov)

;; Returns #t for safe provides. Returns #f for non-provide forms
;; and unsafe provides for which contracts will not be generated.
(define (provide? form)
  (syntax-parse form
    #:literal-sets (kernel-literals)
    [(~and (#%provide . rest) (~not _:unsafe-provide^))
     form]
    [_ #f]))

(define (remove-provides forms)
  (for/list ([e (in-syntax forms)]
             #:unless (provide? e))
    e))

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
(define (generate-prov def-tbl provs pos-blame-id mk-redirect-id)
  ;; Build the final provide with auxilliary definitions
  (with-fresh-mapping
    (for/lists (defs^ export-defs provides aliases)
               ;; sort provs to generate deterministic output
               ([(internal-id external-ids) (in-sorted-free-id-table provs)])
      (define-values (defs^ export-def id alias) (make-quad internal-id def-tbl pos-blame-id mk-redirect-id))
      (define provide-forms
        (for/list ([external-id (in-list external-ids)])
          #`(rename-out [#,id #,external-id])))
      (values #`(begin #,defs^)
              export-def
              #`(provide #,@provide-forms)
              alias))))
