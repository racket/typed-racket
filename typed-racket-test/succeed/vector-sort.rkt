
#lang typed/racket/base

(require racket/vector)

(vector-sort (vector 1 2 3) <)
(vector-sort (vector 1 2 3) < 2 3)
(vector-sort! (vector 1 2 3) <)
(vector-sort! (vector 1 2 3) < 2 3)
