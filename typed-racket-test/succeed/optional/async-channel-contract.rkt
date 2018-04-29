#lang racket/load

;; Test typed-untyped interaction with channels

(module typed typed/racket/optional
  (require typed/racket/async-channel)
  (: ch (Async-Channelof (Boxof Integer)))
  (define ch (make-async-channel))
  (: putter (-> Thread))
  (define (putter)
    (thread (λ () (async-channel-put ch (box 3)))))
  (provide putter ch))

(require 'typed racket/async-channel rackunit)
(putter)

(check-not-exn
  (lambda ()
    (set-box! (async-channel-get ch) "not an integer")))

