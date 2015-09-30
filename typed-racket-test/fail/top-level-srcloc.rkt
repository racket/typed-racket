#;
(exn-pred (regexp-quote "in: (apply + (quote foo))"))
#lang racket/load

;; Test that top-level source locations are recovered in error messages

(require typed/racket)

(apply + 'foo)
