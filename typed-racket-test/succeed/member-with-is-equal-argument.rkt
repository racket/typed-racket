#lang typed/racket

(require typed/rackunit)

;; Test that (member v lst is-equal?) always passes v as the first argument to
;; is-equal? . If this is not the case, the type for the is-equal? argument
;; should be (→ (U a b) (U a b) Any) instead of (→ b a Any).

(let ([needle : Integer
              ;; Use a random needle to prevent some optimizations (but not all)
              (floor (inexact->exact (* (random) 200)))])
  (member needle
          (range 1000)
          (λ ([x : Integer] [y : Integer])
            ;; Check the needle is always the first argument
            (check-equal? x needle)
            ;; Check y = needle implies x = needle
            (check-true (or (not (= y needle)) (= x needle)))
            (= x y))))

;; Test that the third is-equal? argument is taken into account. If it is taken
;; into account, it will return '("c" "ddd" "x"). If it isn't, it will return
;; '("x") instead.
(check-equal? (member "x"
                      '("bb" "c" "ddd" "x")
                      (lambda ([s1 : String] [s2 : String])
                        (= (string-length s1) (string-length s2))))
              '("c" "ddd" "x"))

(check-equal? (member "x" '("bb" "c" "ddd" "x")) '("x"))
