#lang racket/base

;; A custom evt/c for TR that is stricter than the one that
;; comes with Racket. In particular, this will prevent the channel's
;; writing end from being used once it's been exported as an Evtof.

(require racket/contract)

(provide tr:evt/c)

;; tr:evt/c : Contract * -> Contract
(define (tr:evt/c maybe-ctc)
  (define ctc (coerce-contract 'evt/c maybe-ctc))
  (unless (chaperone-contract? ctc)
    (raise-argument-error 'evt/c "chaperone-contract?" ctc))
  (make-tr-evt/c ctc))

;; evt/c-proj : Contract -> (Blame -> Any Any -> Any)
(define (evt/c-late-neg-proj ctc)
  (define real-evt/c (evt/c (tr-evt/c-ctc ctc)))
  (define real-late-neg-proj (contract-late-neg-projection real-evt/c))
  (λ (blame)
    (define real-late-neg-proj* (real-late-neg-proj blame))
    (λ (v neg-party)
      ;; Must not allow a value of type (Evtof X) to be used as
      ;; a value of any type that is invariant in X (i.e., has a
      ;; writing end). For now, this is just channels.
      ;;
      ;; If we support custom evts via struct properties, then
      ;; we may need to tighten this restrictions.
      (if (channel? v)
          (real-late-neg-proj*
           (chaperone-channel
            v
            (λ (ch) (values ch values))
            (λ (ch val)
              (raise-blame-error
               blame ch #:missing-party neg-party
               "cannot put on a channel used as a typed evt")))
           neg-party)
          (real-late-neg-proj* v neg-party)))))

;; evt/c-first-order : Contract -> Any -> Boolean
(define ((evt/c-first-order ctc) v) (evt? v))

;; evt/c-name : Contract -> Sexp
(define (evt/c-name ctc)
  (build-compound-type-name 'evt/c (tr-evt/c-ctc ctc)))

;; evt/c-stronger? : Contract Contract -> Boolean
(define (evt/c-stronger? this that)
  (cond
    [(tr-evt/c? that)
     (define this-ctcs (tr-evt/c-ctc this))
     (define that-ctcs (tr-evt/c-ctc that))
     (contract-stronger? this-ctcs that-ctcs)]
    [else #f]))

(define (evt/c-equivalent? this that)
  (cond
    [(tr-evt/c? that)
     (define this-ctcs (tr-evt/c-ctc this))
     (define that-ctcs (tr-evt/c-ctc that))
     (contract-equivalent? this-ctcs that-ctcs)]
    [else #f]))

(define-struct tr-evt/c (ctc)
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:late-neg-projection evt/c-late-neg-proj
   #:first-order evt/c-first-order
   #:stronger evt/c-stronger?
   #:equivalent evt/c-equivalent?
   #:name evt/c-name))
