#lang racket/base
(require syntax/modresolve syntax/modcollapse (for-template racket/base) racket/match)
(define to-require null)
(define (add-mod! m)
  (set! to-require (cons m to-require)))

(define (do-requires [ns (current-namespace)])
  (parameterize ([current-namespace ns])
    (for ([m (in-list to-require)]
          #:when m)
      ;(eprintf "do-require ~s\n" m)
      (dynamic-require (module-path-index-join '(submod "." #%type-decl) m)
                       #f))))

(define (adjust p)
  (match p
    [`(submod ,(and up (or "." "..")) ,rest ...)
     `(submod ".." ,up . ,rest)]
    [_ p]))

(define (->mp mpi submod)
  (collapse-module-path-index (module-path-index-join `(submod "." ,submod) mpi)))

(define (get-requires)
  (for/list ([m (in-list to-require)]
             #:when m)
    (define path (->mp m '#%type-decl))
    ;; (printf "get-requires: m ~s\n" m)
    ;; (printf "get-requires: joined ~s\n" (module-path-index-join `(submod "." #%type-decl) m))
    ;; (printf "get-requires: collapsed ~s\n" path)
    ;; (printf "get-requires: adjusted ~s\n" (adjust path))
    ;; FIXME: is this really the right code?
    #`(#%require (only #,(adjust path)))))

;; populate the table that tells us what names get us what contracts
(define (do-contract-requires [ns (current-namespace)])
  (parameterize ([current-namespace ns])
    (for ([m (in-list to-require)]
          #:when m)
      (dynamic-require
       (module-path-index-join '(submod "." #%contract-defs-names) m)
       #f))))

;; require the modules that have the definitions of the contracts
(define (get-contract-requires)
  (for/list ([m (in-list to-require)] #:when m)
    #`(#%require (only #,(adjust (->mp m '#%contract-defs))))))

(provide add-mod! do-requires get-requires
         get-contract-requires do-contract-requires)
