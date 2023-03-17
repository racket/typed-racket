#lang typed/racket/base
(require racket/port)

(input-port-append #t (open-input-string "hello") (open-input-string "typed") (open-input-string "racket"))
(input-port-append #t (open-input-string "hello") (open-input-string "typed") (open-input-string "racket")
                   #:name "hello typed racket")
