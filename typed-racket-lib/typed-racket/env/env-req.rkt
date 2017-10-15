#lang racket/base
(require syntax/modresolve syntax/modcollapse (for-template racket/base))
(define to-require null)
(define (add-mod! m)
  (set! to-require (cons m to-require)))

(define (do-requires [ns (current-namespace)])
  (parameterize ([current-namespace ns])
    (for ([m (in-list to-require)]
          #:when m)
      (dynamic-require (module-path-index-join '(submod "." #%type-decl) m)
                       #f))))

(define (get-requires)
  (for/list ([m (in-list to-require)]
             #:when m)
    ;; FIXME: is this really the right code?
    #`(require (only-in #,(collapse-module-path-index (module-path-index-join '(submod "." #%type-decl) m))))))

(define (get-contract-requires)
  (for/list ([m (in-list to-require)]
             #:when m)
    ;; FIXME: is this really the right code?
    #`(require (only-in #,(collapse-module-path-index (module-path-index-join '(submod "." #%contract-defs) m))))))

(provide add-mod! do-requires get-requires get-contract-requires)
