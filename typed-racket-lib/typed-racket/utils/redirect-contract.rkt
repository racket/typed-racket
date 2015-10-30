#lang racket/base

(require syntax/private/modcollapse-noctc (for-template racket/base))
(provide make-make-redirect-to-contract)

;; This is used to define identifiers that expand to a local-require
;; of something else.  It's used to implement identifiers that are
;; protected on export from TR with contracts, but where the
;; contracted defintion is in the #%contract-defs submodule.

;; contract-defs-submod-modidx: a module path index to the contract
;;  defs submodule of the typed module
;;  (using a module path index here makes it friendlier for 'raco exe')

;; id: the name of the export from the submodule which will be
;;     redirected-to by the local-require

;; stx: the syntax object that's the argument to the macro (that is,
;;      the stx object that's the reference to the typed identifier in
;;      a untyped module). The funny eta-expansion with `redirect` is
;;      so that we can recursively invoke it when the redirected id is
;;      used in operator position.

;; This code was originally written by mflatt for the plai-typed
;; language, and then slightly adapted for TR by samth.

(define ((make-make-redirect-to-contract contract-defs-submod-modidx) id)
  (define (redirect stx)
    (cond
     [(identifier? stx)
      (with-syntax ([mp (collapse-module-path-index
                         contract-defs-submod-modidx)]
                    [i (datum->syntax id (syntax-e id) stx stx)])
        (syntax-local-lift-require
         #`(rename mp i #,(datum->syntax #'mp (syntax-e #'i)))
         #'i))]
     [else
      (datum->syntax stx
                     (cons (redirect (car (syntax-e stx)))
                           (cdr (syntax-e stx)))
                     stx
                     stx)]))
  redirect)
