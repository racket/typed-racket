#lang typed/racket

(require typed/rackunit)

;; Test that (assoc v lst is-equal?) always passes v as the first argument to
;; is-equal? . If this is not the case, the type for the is-equal? argument
;; should be (→ (U a c) (U a c) Any) instead of (→ c a Any).
(let ([needle : Integer
              ;; Use a random needle to prevent some optimizations (but not all)
              (floor (inexact->exact (* (random) 200)))])
  (assoc needle
         (ann (map (λ ([x : Integer]) (cons x (format "~a" x))) (range 1000))
              (Listof (Pairof Integer String)))
         (λ ([x : Integer] [y : Integer])
           ;; Check the needle is always the first argument
           (check-equal? x needle)
           ;; Check y = needle implies x = needle
           (check-true (or (not (= y needle)) (= x needle)))
           (= x y))))

;; Test that the third is-equal? argument is taken into account. If it is taken
;; into account, it will return '("c" . 2). If it isn't, it will return
;; '("x" . 4) instead.
(check-equal? (assoc "x"
                     '(("bb" . 1) ("c" . 2) ("ddd" . 3) ("x" . 4))
                     (lambda ([s1 : String] [s2 : String])
                       (= (string-length s1) (string-length s2))))
              '("c" . 2))
