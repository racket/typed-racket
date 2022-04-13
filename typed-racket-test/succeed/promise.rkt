#lang typed/racket

(require typed/rackunit)

;; Test path reasoning for promises

(: foo : (Promise (U Integer String)) -> (U Number False))
(define (foo del)
  (if (integer? (force del))
      (+ 1 (force del))
      (string->number (force del))))

(check-equal? (foo (delay 5)) 6)
(check-equal? (foo (lazy  5)) 6)
(check-equal? (foo (lazy  (delay 5))) 6)
(check-equal? (foo (lazy  (lazy  5))) 6)

(check-equal? (foo (delay "5")) 5)
(check-equal? (foo (lazy  "5")) 5)
(check-equal? (foo (lazy  (delay "5"))) 5)
(check-equal? (foo (lazy  (lazy  "5"))) 5)
