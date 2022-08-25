#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang typed/racket/base/shallow

(module u racket/base
  (provide nats)
  (define nats '(1 2 three four)))

(require/typed 'u
  (nats (Listof Natural)))

(: myadd (-> Natural Natural))
(define (myadd n)
  (add1 n))

(for ((n (in-list nats)))
  (myadd n))
