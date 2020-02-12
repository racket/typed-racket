#lang typed/racket

;; This is a simple typed module with a typed submodule that exports something,
;; so the main test (pr907.rkt) can import from the submodule

(module* m #f
  (provide g)
  (define g 'g))
