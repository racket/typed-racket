#lang typed/racket

(require typed/rackunit)

(check-equal?
 (for/lists ([l1 : (Listof Integer)]
             [l2 : (Listof Integer)]
             #:result (append l1 l2))
            ([x (in-range 3)])
   (values x (add1 x)))
 '(0 1 2 1 2 3))

(check-equal?
 (for/lists : Integer
            ([l1 : (Listof Integer)]
             [l2 : (Listof Integer)]
             #:result (+ (length l1) (length l2)))
            ([x (in-range 3)])
   (values x (add1 x)))
 6)

(check-equal?
 (let-values ([(v1 v2)
               (for/lists ([l1 : (Listof Integer)]
                           [l2 : (Listof Integer)]
                           #:result (values l2 l1))
                          ([x (in-range 3)])
                 (values x (add1 x)))])
   (append v1 v2))
 '(1 2 3 0 1 2))

(check-equal?
 (for/fold ([x : Integer 0]
            #:result (number->string x))
           ([y (in-range 3)])
   (+ x y))
 "3")

(check-equal?
 (for/fold : String
           ([x : Integer 1]
            #:result (number->string x))
           ([y (in-range 3)])
   (+ x y))
 "4")

(check-equal?
 (for*/fold : Integer
            ([x : Integer 0]
             [y : Integer 0]
             #:result (+ x y))
            ([i (in-range 3)]
             [j (in-range 3)])
   (values (+ x i) (+ y i j)))
 27)

(check-equal?
 (for/foldr ([x : Integer 0]
             #:result (number->string x))
            ([y (in-range 3)])
   (+ x y))
 "3")

(check-equal?
 (for/foldr : String
            ([x : Integer 1]
             #:result (number->string x))
            ([y (in-range 3)])
   (+ x y))
 "4")

(check-equal?
 (for*/foldr : Integer
             ([x : Integer 0]
              [y : Integer 0]
              #:result (+ x y))
             ([i (in-range 3)]
              [j (in-range 3)])
  (values (+ x i) (+ y i j)))
 27)
