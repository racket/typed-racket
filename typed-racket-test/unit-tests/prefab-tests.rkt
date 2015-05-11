#lang racket/base

;; Unit tests for prefab type helpres

(require "test-utils.rkt"
         racket/list
         rackunit
         typed-racket/types/prefab)

(provide tests)
(gen-test-main)

(define-check (check-normalize key n norm)
  (check-equal? (normalize-prefab-key key n) norm))

;; check that the abbreviate function is consistent with how
;; Racket abbreviates in the run-time
(define-check (check-abbreviate key n)
  (check-equal? (abbreviate-prefab-key key)
                (prefab-struct-key
                  (apply make-prefab-struct key (make-list n 0)))))

(define tests
 (test-suite
  "Tests for prefab type helpers"
  (check-normalize '(foo) 1 '(foo 1 (0 #f) #()))
  (check-normalize '(foo) 3 '(foo 3 (0 #f) #()))
  (check-normalize '(foo 1) 1 '(foo 1 (0 #f) #()))
  (check-normalize '(foo 3) 3 '(foo 3 (0 #f) #()))
  (check-normalize '(foo bar 3) 4 '(foo 1 (0 #f) #() bar 3 (0 #f) #()))
  (check-normalize '(foo 3 bar 3) 6 '(foo 3 (0 #f) #() bar 3 (0 #f) #()))
  (check-normalize '(foo 1 #()) 1 '(foo 1 (0 #f) #()))
  (check-normalize '(foo 1 #(0)) 1 '(foo 1 (0 #f) #(0)))
  (check-normalize '(foo 5 (0 #f)) 5 '(foo 5 (0 #f) #()))
  (check-normalize '(foo 5 (1 #f)) 6 '(foo 5 (1 #f) #()))
  (check-normalize '(foo 5 (0 #f) #()) 5 '(foo 5 (0 #f) #()))
  (check-normalize '(foo bar 4) 7 '(foo 3 (0 #f) #() bar 4 (0 #f) #()))
  (check-normalize '(foo (1 #f) bar 4) 8 '(foo 3 (1 #f) #() bar 4 (0 #f) #()))
  (check-normalize '(foo #(1) bar 4) 7 '(foo 3 (0 #f) #(1) bar 4 (0 #f) #()))
  (check-normalize '(foo bar 4 (1 #f)) 8 '(foo 3 (0 #f) #() bar 4 (1 #f) #()))
  (check-normalize '(foo bar 1 baz 4 (1 #f))
                   8
                   '(foo 2 (0 #f) #() bar 1 (0 #f) #() baz 4 (1 #f) #()))

  (check-equal? (prefab-key->field-count '(foo 1 (0 #f) #()))
                1)
  (check-equal? (prefab-key->field-count '(foo 1 (1 #f) #()))
                2)
  (check-equal? (prefab-key->field-count
                 '(foo 1 (1 #f) #() bar 1 (0 #f) #()))
                3)
  (check-equal? (prefab-key->field-count
                 '(foo 1 (1 #f) #() bar 1 (0 #f) #() baz 2 (0 #f) #()))
                5)

  (check-abbreviate '(foo) 1)
  (check-abbreviate '(foo 1) 1)
  (check-abbreviate '(foo 3) 3)
  (check-abbreviate '(foo bar 3) 4)
  (check-abbreviate '(foo 3 bar 3) 6)
  (check-abbreviate '(foo 1 #()) 1)
  (check-abbreviate '(foo 1 #(0)) 1)
  (check-abbreviate '(foo 5 (0 #f)) 5)
  (check-abbreviate '(foo 5 (1 #f)) 6)
  (check-abbreviate '(foo 5 (0 #f) #()) 5)
  (check-abbreviate '(foo 5 (1 #f) #()) 6)
  (check-abbreviate '(foo 5 (0 #f) #(1)) 5)
  (check-abbreviate '(foo 5 (1 #f) #(1)) 6)
  (check-abbreviate '(foo 5 (0 #f) #() bar 1) 6)
  (check-abbreviate '(foo 5 (0 #f) #(1) bar 1) 6)
  (check-abbreviate '(foo 5 (1 #f) #(1) bar 1) 7)
  (check-abbreviate '(foo 5 (1 #f) #(1) bar 1 #()) 7)
  (check-abbreviate '(foo 5 (0 #f) #(1) bar 1 #()) 6)
  (check-abbreviate '(foo 5 (0 #f) #() bar 1 #()) 6)
  (check-abbreviate '(foo 5 (1 #f) #(1) bar 1 #(0)) 7)

  (check-true (prefab-key-subtype? '(foo 1 (0 #f) #() bar 1 (0 #f) #())
                                   '(bar 1 (0 #f) #())))
  (check-false (prefab-key-subtype? '(foo 1 (0 #f) #())
                                    '(bar 1 (0 #f) #())))

  (check-equal? (prefab-key->field-mutability
                 '(foo 1 (0 #f) #() bar 1 (0 #f) #()))
                (list #f #f))
  (check-equal? (prefab-key->field-mutability
                 '(foo 2 (0 #f) #(0) bar 3 (0 #f) #(1)))
                (list #f #t #f #t #f))
  (check-equal? (prefab-key->field-mutability
                 '(foo 2 (0 #f) #(0) bar 3 (0 #f) #(1) baz 1 (0 #f) #(0)))
                (list #t #f #t #f #t #f))))
