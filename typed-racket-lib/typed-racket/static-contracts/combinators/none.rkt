#lang racket/base

;; Static contract for none/c.
;; Allows optimizations as many combinators can be simplified if their arguments are none/sc
;; Ex: (listof/sc none/sc) => null?/sc

(require "../../utils/utils.rkt"
         "../structures.rkt" "../constraints.rkt"
         racket/match
         (contract-req)
         (for-template racket/base racket/contract/base)
         (for-syntax racket/base syntax/parse))

(provide none/sc:)
(provide/cond-contract
 [none/sc static-contract?])


;;Printing
(define (none-write-proc v port mode)
  (if (equal? mode 0)
      (display "none/sc" port)
      (display "#<none/sc>" port)))

(struct none-combinator combinator ()
        #:transparent
        #:methods gen:sc
          [(define (sc-map v f) v)
           (define (sc-traverse v f) (void))
           (define (sc->contract v f) #'none/c)
           (define (sc->constraints v f) (simple-contract-restrict 'flat))]
        #:methods gen:custom-write [(define write-proc none-write-proc)])

(define-match-expander none/sc:
  (syntax-parser
    [(_) #'(? none-combinator?)]))

(define none/sc (none-combinator null))

