#lang racket/base

;; Test custom vector contracts:
;; 1. accept & reject the right kinds of values
;; 2. don't overlap in an or/c

(module+ test
  (require rackunit
           racket/contract
           typed-racket/utils/vector-contract)

  (test-case "vector-contract:correct"
    (define I-vector-ctc (immutable-vector/c symbol?))
    (define I-vectorof-ctc (immutable-vectorof/c symbol?))
    (define M-vector-ctc (mutable-vector/c symbol?))
    (define M-vectorof-ctc (mutable-vectorof/c symbol?))

    (define I-val '#(A))
    (define M-val (vector 'A))

    (check-not-exn
      (λ () (contract I-vector-ctc I-val '+ '-)))
    (check-not-exn
      (λ () (contract I-vectorof-ctc I-val '+ '-)
            (contract I-vectorof-ctc '#() '+ '-)))
    (check-not-exn
      (λ () (contract M-vector-ctc M-val '+ '-)))
    (check-not-exn
      (λ () (contract M-vectorof-ctc M-val '+ '-)))

    (check-exn exn:fail:contract?
      (λ () (contract I-vector-ctc M-val '+ '-)
            (contract I-vector-ctc '#() '+ '-)))
    (check-exn exn:fail:contract?
      (λ () (contract I-vectorof-ctc M-val '+ '-)))
    (check-exn exn:fail:contract?
      (λ () (contract M-vector-ctc I-val '+ '-)
            (contract M-vector-ctc (vector) '+ '-)))
    (check-exn exn:fail:contract?
      (λ () (contract M-vectorof-ctc I-val '+ '-))))

  (test-case "vector-contract:overlap"
    (let ([ctc (or/c (immutable-vectorof/c integer?) (mutable-vectorof/c integer?))])
      (check-not-exn
        (λ () (contract ctc (vector) '+ '-))))

    (let ([ctc (or/c (immutable-vector/c integer?) (mutable-vector/c integer?))])
      (check-not-exn
        (λ () (contract ctc (vector 2) '+ '-))))))
