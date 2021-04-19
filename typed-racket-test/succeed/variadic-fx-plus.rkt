#lang typed/racket

(require racket/fixnum racket/unsafe/ops typed/rackunit)

(check-equal? (fx+) 0)
(check-equal? (fx+ 7) 7)
(check-equal? (fx+ 1 2) 3)
(check-equal? (fx+ 1 2 3 4) 10)
(check-equal? (apply fx+ '(1 2 3 4)) 10)
(check-equal? (ann (apply fx+ '(-1 -2 -3)) Nonpositive-Fixnum) -6)

(check-equal? (unsafe-fx+) 0)
(check-equal? (unsafe-fx+ 7) 7)
(check-equal? (unsafe-fx+ 1 2) 3)
(check-equal? (unsafe-fx+ 1 2 3 4) 10)
(check-equal? (apply unsafe-fx+ '(1 2 3 4)) 10)
(check-equal? (ann (apply unsafe-fx+ '(-1 -2 -3)) Nonpositive-Fixnum) -6)
