#lang typed/racket

(require racket/treelist)

(define tl (treelist 0 1 2 3))

(treelist-empty? tl)

(treelist-length tl)

(treelist-member? tl 1)

(treelist-first tl)

(treelist-rest tl)

(treelist-last tl)

(treelist-add tl 1)

(treelist-cons tl 1)

(treelist-delete tl 1)

(make-treelist 5 1)

(treelist-ref tl 1)

(treelist-insert tl 1 1)

(treelist-set tl 0 1)

(treelist-take tl 2)

(treelist-take tl 2)
(treelist-drop tl 2)
(treelist-take-right tl 2)
(treelist-drop-right tl 2)

(treelist-sublist tl 1 3)

(treelist-reverse tl)

(treelist->list tl)
(list->treelist (list 0 1 2 3))

(treelist->vector tl)
(vector->treelist (vector 0 1 2 3))

(treelist? treelist)

(treelist-append tl tl tl)

(treelist-map tl (λ ([x : Byte]) (+ x 1)))

(treelist-for-each tl  (λ ([x : Byte]) (+ x 1)))

(treelist-filter (λ ([x : Byte]) (equal? x 1)) tl)

(treelist-find tl (λ ([x : Byte]) (equal? x 1)))

(treelist-index-of tl 3)
(treelist-index-of tl 3 equal?)

(treelist-flatten (treelist tl tl))

(treelist-append (treelist (treelist tl) tl))

(treelist-sort tl >)