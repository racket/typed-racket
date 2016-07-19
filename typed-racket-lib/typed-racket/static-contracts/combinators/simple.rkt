#lang racket/base

;; Static contracts that are terminal and have no sub parts.
;; Unlike contracts defined with define-terminal-contract, equality of these contracts is based solely
;; on identity. Thus they are most useful for contracts which have no meaningful structure.
;; Ex: (flat/sc #'number?)

(require
  "../structures.rkt"
  "../constraints.rkt"
  racket/match
  racket/contract)

(provide
  (contract-out
    [flat/sc ((syntax?) ((or/c #f any/c)) . ->* . static-contract?)]
    [chaperone/sc ((syntax?) ((or/c #f any/c)) . ->* . static-contract?)]
    [impersonator/sc ((syntax?) ((or/c #f any/c)) . ->* . static-contract?)]))

(define (simple-contract-write-proc v port mode)
  (match-define (simple-contract syntax kind name) v)
  (define-values (open close)
    (if (equal? mode 0)
        (values "(" ")")
        (values "#<" ">")))
  (display open port)
  (fprintf port "~a/sc" kind)
  (display " " port)
  (write (or name (syntax->datum syntax)) port)
  (display close port))


;; check equality of two syntax objects by structural traversal
;; where identifiers are compared by free-identifier=?
(define (stx-equal? s1 s2 recur)
  (define d1 (if (syntax? s1) (syntax-e s1) s1))
  (define d2 (if (syntax? s2) (syntax-e s2) s2))
  (cond [(and (identifier? s1) (identifier? s2))
         (free-identifier=? s1 s2)]
        [(and (pair? d1) (pair? d2))
         (and (stx-equal? (car d1) (car d2) recur)
              (stx-equal? (cdr d1) (cdr d2) recur))]
        [else (recur d1 d2)]))

(struct simple-contract static-contract (syntax kind name)
        #:transparent
        #:methods gen:equal+hash
         [(define (equal-proc s1 s2 recur)
            (and ;; have to make sure identifiers are compared by free-id=?
                 ;; because of struct predicates
                 (stx-equal? (simple-contract-syntax s1)
                             (simple-contract-syntax s2)
                             recur)
                 (recur (simple-contract-kind s1)
                        (simple-contract-kind s2))
                 (recur (simple-contract-name s1)
                        (simple-contract-name s2))))
          (define (hash-proc sc hash-code)
            (hash-code (list (syntax->datum (simple-contract-syntax sc))
                             (simple-contract-kind sc)
                             (simple-contract-name sc))))
          (define (hash2-proc sc hash-code)
            (hash-code (list (syntax->datum (simple-contract-syntax sc))
                             (simple-contract-kind sc)
                             (simple-contract-name sc))))]
        #:methods gen:sc
         [(define (sc-map v f) v)
          (define (sc-traverse v f) (void))
          (define (sc->contract v f) (simple-contract-syntax v))
          (define (sc->constraints v f) (simple-contract-restrict (simple-contract-kind v)))
          (define (sc-terminal-kind v) (simple-contract-kind v))]
        #:methods gen:custom-write [(define write-proc simple-contract-write-proc)])

(define (flat/sc ctc [name #f])
  (simple-contract ctc 'flat name))
(define (chaperone/sc ctc [name #f])
  (simple-contract ctc 'chaperone name))
(define (impersonator/sc ctc [name #f])
  (simple-contract ctc 'impersonator name))
