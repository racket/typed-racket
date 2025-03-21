#lang typed/racket

(require racket/treelist)

(define-predicate string-treelist? (TreeListof String))

(string-treelist? (treelist 1 2 3))

(string-treelist? (treelist "1" "2" "3"))
