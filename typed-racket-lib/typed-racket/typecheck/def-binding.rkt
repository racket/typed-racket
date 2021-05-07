#lang racket/base

(require racket/generic
         racket/match
         racket/syntax
         racket/struct-info
         syntax/id-table
         (for-syntax racket/base)
         (for-template racket/base)
         "../utils/utils.rkt"
         (contract-req)
         (utils tc-utils struct-info)
         (types struct-table)
         (private syntax-properties)
         (typecheck renamer))

(provide with-fresh-mapping make-quad)

(require-for-cond-contract racket/struct-info)

(define (freshen-id id)
  ((make-syntax-introducer) id))

(define-generics providable
  [mk-quad providable new-id def-tbl pos-blame-id mk-indirect-id])

(define (mk-ignored-quad i)
  (values #'(begin) #'(begin) i null))


;; maps ids defined in this module to an identifier which is the possibly-contracted version of the key
(define mapping (make-parameter #f))

(define-struct binding (name) #:transparent)

(define-struct (def-binding binding) (ty)
  #:transparent
  #:methods gen:providable
  [(define/cond-contract (mk-quad me new-id def-tbl pos-blame-id mk-redirect-id)
     (-> providable? identifier? (free-id-table/c identifier? binding? #:immutable #t)
           identifier? identifier?
           (values syntax? syntax? identifier? (listof (list/c identifier? identifier?))))
     (match-define (def-binding internal-id ty) me)
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
                 (define-syntax export-id
                   (make-typed-renaming #'id #'local-untyped-id)))
        new-id
        null)))])

(define-struct (def-stx-binding binding) () #:transparent
  #:methods gen:providable
  [(define/cond-contract (mk-quad me new-id def-tbl pos-blame-id mk-redirect-id)
     (-> providable? identifier? (free-id-table/c identifier? binding? #:immutable #t)
           identifier? identifier?
           (values syntax? syntax? identifier? (listof (list/c identifier? identifier?))))
     (match-define (def-stx-binding internal-id) me)
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
            (define-syntax export-id
              (make-typed-renaming #'id #'untyped-id)))
        new-id
        (list (list #'export-id #'id)))))])


(define-struct (def-struct-stx-binding def-stx-binding)
  (sname tname static-info constructor-name constructor-type extra-constr-name)
  #:transparent
  #:methods gen:providable
  [(define/generic super-mk-quad mk-quad)
   (define/cond-contract (mk-quad me new-id def-tbl pos-blame-id mk-redirect-id)
     (-> providable? identifier? (free-id-table/c identifier? binding? #:immutable #t)
           identifier? identifier?
           (values syntax? syntax? identifier? (listof (list/c identifier? identifier?))))
     (define (mk internal-id)
       (make-quad internal-id def-tbl pos-blame-id mk-redirect-id))

     (match-define (def-struct-stx-binding internal-id sname tname si constr-name^ constr-type extra-constr-name) me)
     (match-define (list type-desc _ pred (list accs ...) muts super) (extract-struct-info si))
     (define-values (defns export-defns new-ids aliases)
       (for/lists (defns export-defns new-ids aliases)
                  ([e (in-list (list* type-desc pred super accs))])
         (if (identifier? e)
             (mk e)
             (mk-ignored-quad e))))

     (define sname-is-constructor? (and (or extra-constr-name (free-identifier=? sname constr-name^)) #t))

     (define constr-name (or extra-constr-name constr-name^))
     ;; Here, we recursively handle all of the identifiers referenced
     ;; in this static struct info.
     (define-values (constr-defn constr-export-defn constr-new-id constr-aliases)
       (cond
         [(not (identifier? constr-name))
          (values #'(begin) #'(begin) #f null)]
         ;; avoid generating the quad for constr twice.
         ;; skip it when the binding is for the type name
         [(and (free-identifier=? internal-id sname) (free-identifier=? constr-name internal-id))
          (super-mk-quad (make-def-binding constr-name constr-type) (freshen-id constr-name) def-tbl pos-blame-id mk-redirect-id)]
         [else
          (make-quad constr-name def-tbl pos-blame-id mk-redirect-id)]))

     (define/with-syntax (constr* type-desc* pred* super* accs* ...)
       (for/list ([i (in-list (cons constr-new-id new-ids))])
         (and (identifier? i) #`(quote-syntax #,i))))

     (with-syntax* ([id internal-id]
                    [export-id new-id]
                    [protected-id (freshen-id #'id)]
                    ;; when the struct name is also the constructor name, we put
                    ;; the former in the struct info, because it is the
                    ;; exporting binding. Otherwise, we put the latter in the
                    ;; struct info
                    [constr-in-si* (if (free-identifier=? new-id constr-new-id)
                                       new-id
                                       constr-new-id)]
                    [type-name tname])
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
              (let ((info (list type-desc* (syntax constr-in-si*) pred* (list accs* ...)
                                (list #,@(map (lambda (x) #'#f) accs)) super*)))
                (make-struct-info-wrapper* constr* info (syntax type-name) #,sname-is-constructor?)))
            (define-syntax export-id
              (make-rename-transformer #'protected-id)))
        #'export-id
        (cons (list #'export-id internal-id)
              (apply append constr-aliases aliases)))))])

(define-syntax-rule (with-fresh-mapping . body)
  (parameterize ([mapping (make-free-id-table)])
    . body))

;; `make-quad` and the generic interface `mk-quad` return value four values:
;; First return value is a syntax object of definitions, which will go in
;;    the #%contract-defs submodule
;; Second is a syntax object of definitions to go in the main module, including
;;    the defintion to be exported
;; Third is the id to export
;; Fourth is a list of two element lists representing type aliases
;;
;; When generating quads for a module, `make-quad` must be used inside a
;; `start-making-quad` expression with proper initializations of parameters
;; def-tbl, pos-blame-id, and mk-redirect-id
(define/cond-contract (make-quad internal-id def-tbl pos-blame-id mk-redirect-id)
  (-> identifier? (free-id-table/c identifier? binding? #:immutable #t) identifier? identifier?
      (values syntax? syntax? identifier? (listof (list/c identifier? identifier?))))
  (define new-id (freshen-id internal-id))
  (cond
    ;; if it's already done, do nothing
    [(free-id-table-ref (mapping) internal-id
                        ;; if it wasn't there, put it in, and skip this case
                        (λ () (free-id-table-set! (mapping) internal-id new-id) #f))
     => (lambda (n)
          (mk-ignored-quad n))]
    [(prefab-struct-field-operator? internal-id)
     (mk-ignored-quad internal-id)]
    [(free-id-table-ref def-tbl internal-id #f)
     =>
     (lambda (ins)
       (mk-quad ins new-id def-tbl pos-blame-id mk-redirect-id))]
    [else
     ;; otherwise, not defined in this module, not our problem
     (mk-ignored-quad internal-id)]))

(provide/cond-contract
 (struct binding ([name identifier?]))
 (struct (def-binding binding) ([name identifier?] [ty any/c]))
 (struct (def-stx-binding binding) ([name identifier?]))
 (struct (def-struct-stx-binding binding) ([name identifier?]
                                           [sname identifier?]
                                           [tname identifier?]
                                           [static-info (or/c #f struct-info?)]
                                           [constructor-name identifier?]
                                           [constructor-type any/c]
                                           [extra-constr-name (or/c #f identifier?)])))
