;; see Typed Racket GH issue #640
#lang typed/racket #:with-refinements

(: lyst : (-> ([arg : Integer])
              (Refine [result : (List Integer)]
                      (= (car result) arg))))
(define (lyst arg)
  (define result (list arg))
  (assert (= (car result) arg))
  result)

(: lyst-car : (-> ([lyst : (List Integer)])
                  (Refine [result : Integer]
                          (= result (car lyst)))))
(define (lyst-car lst)
  (car lst))

(: lyst+ : (-> ([a : (List Integer)]
                [b : (List Integer)])
               (Refine [result : (List Integer)]
                       (= (car result) (+ (car a) (car b))))))
(define (lyst+ a b)
  (lyst (+ (lyst-car a) (lyst-car b))))

(: lyst1+ : (-> ([a : (List Integer)]
                 [b : (List Integer)])
                (Refine [result : (List Integer)]
                        (= (car result) (+ (car a) (car b))))))
(define (lyst1+ a b)
  (define res (lyst (+ (lyst-car a) (lyst-car b))))
  res)


(: lyst2+ : (-> ([a : (List Integer)]
                 [b : (List Integer)])
                (values (Refine [result : (List Integer)]
                                (= (car result) (+ (car a) (car b))))
                        (Refine [result : (List Integer)]
                                (= (car result) (+ (car a) (car b)))))))
(define (lyst2+ a b)
  (values (lyst (+ (lyst-car a) (lyst-car b)))
          (lyst (+ (lyst-car a) (lyst-car b)))))

