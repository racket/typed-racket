#lang racket/base

;; Test that a typed modules can unsafely provide another typed module's export

;; A : safely provide a typed value
(module A typed/racket/base
  (define b : (Boxof Natural) (box 1))
  (provide b))

;; B : unsafely provide the value from A
(module B typed/racket/base
  (require typed/racket/unsafe)
  (require (submod ".." A))
  (unsafe-provide b))

;; C : check that modules A and B export different identifiers that expand to the same identifier
(module C typed/racket
  (require (prefix-in A (submod ".." A)))
  (require (prefix-in B (submod ".." B)))
  (define-syntax (check-ids stx)
    (syntax-case stx ()
     [(_ a-id b-id)
      (with-syntax ([different-before? (not (free-identifier=? #'a-id #'b-id))]
                    [same-after? (free-identifier=? (local-expand #'a-id 'expression #f) (local-expand #'b-id 'expression #f))])
        #'(values different-before? same-after?))]))
  (define-values [different-before? same-after?] (check-ids Ab Bb))
  (provide different-before? same-after?))

(require 'B 'C rackunit)

(check-not-exn (lambda () (set-box! b -1)))
(check-equal? -1 (unbox b))
(check-true different-before?)
(check-true same-after?)
