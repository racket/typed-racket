#lang typed/racket

(require math/array typed/rackunit)

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


;; based on a question by 曹朝 on the Racket Users mailing list:
;; https://groups.google.com/d/msg/racket-users/0tOGWZ9O57c/jRXJYkUdAQAJ
(: shortest-edit-distance (-> String String (U #f Integer)))
(define (shortest-edit-distance str0 str1)
(let* ([l0 : Integer (string-length str0)]
       [l1 : Integer (string-length str1)]
       [table : (Mutable-Array Integer) (array->mutable-array (make-array (vector l0 l1) 0))])
  (for*/last : (U #f Integer) ([i0 : Integer (in-range l0)]
                               [i1 : Integer (in-range l1)])
    (let* ([c0 : Char (string-ref str0 i0)]
           [c1 : Char (string-ref str1 i1)]
           [base : Integer (cond
                             [(and (= i0 0) (= i1 0)) 0]
                             [(= i0 0) (array-ref table (vector i0 (sub1 i1)))]
                             [(= i1 0) (array-ref table (vector (sub1 i0) i1))]
                             [else (min (array-ref table (vector i0 (sub1 i1)))
                                        (array-ref table (vector (sub1 i0) i1))
                                        (array-ref table (vector (sub1 i0) (sub1 i1))))])]
           [answer : Integer (if (char=? c0 c1) base (add1 base))])
      (array-set! table (vector i0 i1) answer)
      answer))))
