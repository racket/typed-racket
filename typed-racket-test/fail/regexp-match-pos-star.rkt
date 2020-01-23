#lang typed/racket/base #:no-optimize

(lambda ()
  (let ([problem (regexp-match-positions* #rx"." "")])
    (and problem (ann problem (Pairof Any Any)))))
