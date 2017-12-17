#lang racket/base
(require syntax/modresolve syntax/modcollapse (for-template racket/base) racket/match)
(define to-require null)
(define (add-mod! m)
  (set! to-require (cons m to-require)))

;; produce code for all the requires we need to load types
(define (get-requires)
  (for/list ([m (in-list to-require)]
             #:when m)
    (define path (->mp m '#%type-decl))
    #`(#%require (only #,(adjust path)))))

;; dynamically do all of the above requires
;; populates the type name tables
(define (do-requires [ns (current-namespace)])
  (parameterize ([current-namespace ns])
    (for ([m (in-list to-require)]
          #:when m)
      (dynamic-require (module-path-index-join '(submod "." #%type-decl) m)
                       #f))))

;; adjust: require-spec -> require-spec
;; rewrite a spec that works in a module M to one that works in a submodule of M
(define (adjust p)
  (match p
    [`(submod ,(and up (or "." "..")) ,rest ...)
     `(submod ".." ,up . ,rest)]
    [_ p]))

;; ->mp : module-path-index? symbol? -> module-path-index?
;; combine module-path-index with a submodule, producing an sexp we can manipulate
(define (->mp mpi submod)
  (collapse-module-path-index (module-path-index-join `(submod "." ,submod) mpi)))

;; generate code to require the modules that have the definitions of the contracts
(define (get-contract-requires)
  (for/list ([m (in-list to-require)] #:when m)
    #`(#%require (only #,(adjust (->mp m '#%contract-defs))))))

;; dynamically do the above requires
;; populates the table that tells us what names get us what contracts
(define (do-contract-requires [ns (current-namespace)])
  (parameterize ([current-namespace ns])
    (for ([m (in-list to-require)]
          #:when m)
      (dynamic-require
       (module-path-index-join '(submod "." #%contract-defs-names) m)
       #f))))

(provide add-mod! do-requires get-requires
         get-contract-requires do-contract-requires)
