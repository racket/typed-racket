#lang typed/racket #:with-refinements


(provide add-to-one)


(: add-to-one (-> (Refine [o : Integer] (= o 1))
                  Integer
                  Integer))
(define (add-to-one one n)
  (+ one n))



(ann 42 (Refine [n : Integer] (= n 42)))


;; constant comparisons


(if (< 5 4)
    (+ "Luke," "I am your father")
    "that's impossible!")


(if (<= 5 4)
    (+ "Luke," "I am your father")
    "that's impossible!")


(if (= 5 4)
    (+ "Luke," "I am your father")
    "that's impossible!")

(if (>= 4 5)
    (+ "Luke," "I am your father")
    "that's impossible!")

(if (> 4 5)
    (+ "Luke," "I am your father")
    "that's impossible!")



;; arithmetic op comparisons

(if (< (+ 2 3) 4)
    (+ "Luke," "I am your father")
    "that's impossible!")


(if (<= (+ 2 3) (* 2 2))
    (+ "Luke," "I am your father")
    "that's impossible!")


(if (= (+ 2 3) 4)
    (+ "Luke," "I am your father")
    "that's impossible!")

(if (>= 4 (+ 2 3))
    (+ "Luke," "I am your father")
    "that's impossible!")

(if (> (* 2 2) (+ 2 3))
    (+ "Luke," "I am your father")
    "that's impossible!")


;; module level bound to integers

(define four 4)

(if (< (+ 2 3) four)
    (+ "Luke," "I am your father")
    "that's impossible!")


(if (<= (+ 2 3) four)
    (+ "Luke," "I am your father")
    "that's impossible!")


(if (= (+ 2 3) four)
    (+ "Luke," "I am your father")
    "that's impossible!")

(if (>= four (+ 2 3))
    (+ "Luke," "I am your father")
    "that's impossible!")

(if (> four (+ 2 3))
    (+ "Luke," "I am your father")
    "that's impossible!")

;; module level bound to expr

(define two+two (+ 2 2))

(if (< (+ 2 3) two+two)
    (+ "Luke," "I am your father")
    "that's impossible!")


(if (<= (+ 2 3) two+two)
    (+ "Luke," "I am your father")
    "that's impossible!")


(if (= (+ 2 3) two+two)
    (+ "Luke," "I am your father")
    "that's impossible!")

(if (>= two+two (+ 2 3))
    (+ "Luke," "I am your father")
    "that's impossible!")

(if (> two+two (+ 2 3))
    (+ "Luke," "I am your father")
    "that's impossible!")


;; let binding bound to integers

(let ([four four]
      [other-four 4]
      [two+two (+ 2 2)])
  
  (if (< (+ 2 3) four)
      (+ "Luke," "I am your father")
      "that's impossible!")


  (if (<= (+ 2 3) four)
      (+ "Luke," "I am your father")
      "that's impossible!")


  (if (= (+ 2 3) four)
      (+ "Luke," "I am your father")
      "that's impossible!")

  (if (>= four (+ 2 3))
      (+ "Luke," "I am your father")
      "that's impossible!")

  (if (> four (+ 2 3))
      (+ "Luke," "I am your father")
      "that's impossible!")




  (if (< (+ 2 3) other-four)
      (+ "Luke," "I am your father")
      "that's impossible!")


  (if (<= (+ 2 3) other-four)
      (+ "Luke," "I am your father")
      "that's impossible!")


  (if (= (+ 2 3) other-four)
      (+ "Luke," "I am your father")
      "that's impossible!")

  (if (>= other-four (+ 2 3))
      (+ "Luke," "I am your father")
      "that's impossible!")

  (if (> other-four (+ 2 3))
      (+ "Luke," "I am your father")
      "that's impossible!")


  

  (if (< (+ 2 3) two+two)
      (+ "Luke," "I am your father")
      "that's impossible!")


  (if (<= (+ 2 3) two+two)
      (+ "Luke," "I am your father")
      "that's impossible!")


  (if (= (+ 2 3) two+two)
      (+ "Luke," "I am your father")
      "that's impossible!")

  (if (>= two+two (+ 2 3))
      (+ "Luke," "I am your father")
      "that's impossible!")

  (if (> two+two (+ 2 3))
      (+ "Luke," "I am your father")
      "that's impossible!"))



(define (foo [four : (Refine [val : Integer] (= val 4))])
  (if (< (+ 2 3) four)
      (+ "Luke," "I am your father")
      "that's impossible!")


  (if (<= (+ 2 3) four)
      (+ "Luke," "I am your father")
      "that's impossible!")


  (if (= (+ 2 3) four)
      (+ "Luke," "I am your father")
      "that's impossible!")

  (if (>= four (+ 2 3))
      (+ "Luke," "I am your father")
      "that's impossible!")

  (if (> four (+ 2 3))
      (+ "Luke," "I am your father")
      "that's impossible!"))