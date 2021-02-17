#;
(exn-pred #rx"expected: \\(Prefab container String\\).*given: \\(Prefab container Positive-Byte\\)")
#lang typed/racket/base

(define val '#s(container 10))
(struct container
    ([value : String])
    #:prefab)
  (ann val container)

