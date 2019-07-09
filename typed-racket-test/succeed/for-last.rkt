#lang typed/racket

(require typed/rackunit)

(check-equal? (for/last : (U String #f) ([i '(1 2 3 4 5)]
                                         #:when (even? i))
                 (number->string i))
              "4")

(check-equal? (for/last ([i '()])
                (error "doesn't get here"))
              #f)

(check-equal? (for*/last : (U (List Integer Char) #f) ([i '(1 2)]
                                                       [j "ab"])
                (list i j))
              (list 2 #\b))

(check-equal? (for*/last : (U (List Integer Integer) #f) ([i (in-range 5)]
                                                          #:when (odd? i)
                                                          [j (in-range 4)]
                                                          #:unless (= i j))
                (list i j))
              (list 3 2))

