#lang typed/racket

(require typed/rackunit)

(check-equal? (index-of '(a 12 c d) 'd) 3)
(check-equal? (index-of '(a 12 c d) 'e) #f)
(check-equal? (index-of
               '("ab" "cde" "fhgi")
               3
               (λ ([str : String] [len : Integer])
                 (= (string-length str) len)))
              1)
