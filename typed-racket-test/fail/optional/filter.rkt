#;
(exn-pred exn:fail:contract? #rx"add1")

#lang typed/racket/base/optional

(module u racket/base
  (provide nats)
  (define nats '(1 2 three four)))

(require/typed 'u
  (nats (Listof Natural)))

(: myadd (-> Natural Natural))
(define (myadd n)
  (add1 n))

(filter myadd nats)
