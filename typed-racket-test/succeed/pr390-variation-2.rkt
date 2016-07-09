#lang racket/base

;; Reported by John Clements
;; https://groups.google.com/forum/#!searchin/racket-users/trie$20functions/racket-users/WBPCsdae5fs/J7CIOeV-CQAJ

(require pfds/trie)

(define (main)
  (define (rand-list)
    (for/list ([i (in-range 128)])
        (random 256)))
  (define t (trie (list (rand-list))))
  (bind (rand-list) 0 t))

;; Expect to see about 1 millisecond when typed and 12 seconds when untyped
(call-with-limits 2 3
  main)

