#lang typed/racket

;; Test vector ops,
;;  check that returned vectors are mutable/immutable as docs say

(module+ test
  (require typed/rackunit)

  (test-case "vector-filter"
    (check-false (immutable? (vector-filter even? (vector-immutable 1 2 3 4))))
    (check-false (immutable? (vector-filter even? (vector 1 2 3 4)))))

  (test-case "vector-filter-not"
    (check-false (immutable? (vector-filter-not even? (vector-immutable 1 2 3 4))))
    (check-false (immutable? (vector-filter-not even? (vector 1 2 3 4)))))

  (test-case "vector-copy!"
    (let ([dst (vector 0 0 0)])
      (vector-copy! dst 0 (vector 2 4))
      (check-equal? dst (vector 2 4 0))))

  (test-case "vector-copy"
    (check-false (immutable? (vector-copy '#(1 2 3))))
    (check-false (immutable? (vector-copy (vector 0 0 0)))))

  (test-case "vector-map"
    (check-false (immutable? (vector-map + (vector 1 2 3) '#(1 2 3)))))

  (test-case "vector-map!"
    (let ([v (vector 1 2 3)])
      (vector-map! + v '#(1 2 3))
      (check-equal? v (vector 2 4 6))
      (check-false (immutable? v))))

  (test-case "vector-append"
    (check-false (immutable? (vector-append '#(0 1 2) '#(3 4 5)))))

  (test-case "vector-take*"
    (define vi (vector-immutable 1 2 3))
    (define vm (vector 1 2 3))

    (check-false (immutable? (vector-take vi 2)))
    (check-false (immutable? (vector-take vm 2)))
    (check-false (immutable? (vector-drop vi 2)))
    (check-false (immutable? (vector-drop vm 2)))
    (check-false (immutable? (vector-take-right vi 2)))
    (check-false (immutable? (vector-take-right vm 2)))
    (check-false (immutable? (vector-drop-right vi 2)))
    (check-false (immutable? (vector-drop-right vm 2)))

    (let-values (((a b) (vector-split-at vi 1)))
      (check-false (immutable? a))
      (check-false (immutable? b)))
    (let-values (((a b) (vector-split-at vm 1)))
      (check-false (immutable? a))
      (check-false (immutable? b)))
    (let-values (((a b) (vector-split-at-right vi 1)))
      (check-false (immutable? a))
      (check-false (immutable? b)))
    (let-values (((a b) (vector-split-at-right vm 1)))
      (check-false (immutable? a))
      (check-false (immutable? b)))
  )

  (test-case "for/vector"
    (check-false (immutable? (for/vector ([x : Real (in-list '(1 2 3))]) x))))

  (struct foo ([x : Integer]))
  (test-case "struct->vector"
    (check-false (immutable? (struct->vector (foo 1)))))

  (test-case "object->vector"
    (check-false (immutable? (object->vector (new object%)))))

  (test-case "current-command-line-arguments"
    (check-false (immutable? (ann (current-command-line-arguments) (Mutable-Vectorof String)))))
)
