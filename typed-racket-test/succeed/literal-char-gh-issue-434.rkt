#lang typed/racket

;; The precise type for chars should be the char itself:
(ann #\nul #\nul)
;; Up-casting as Char should still work:
(ann #\nul Char)
(ann (ann #\b '#\b) Char)

;; Check that the inferred type is still implicitly widened to Char by default,
;; for backwards compatibility (previously, all chars had the type Char):
;; * Check that when passed as a #:∀ type, the inferred type is Char
(ann (let #:∀ (C) ([c : C #\a])
         (λ ([x : C]) x))
       (→ Char Char))
;; * Check that loops which rely on the first iteration having the wider Char
;;   type still work:
(let loop : Void ([c #\a])
    (if (equal? c #\a)
        (loop #\b)
        (void)))

(define v (vector #\a #\b #\c))
(define b (box #\a))
(define c (ann (box (ann #\a '#\a)) (Boxof '#\a)))

(vector-set! v 0 #\d)
(set-box! b #\z)
;; (set-box! c #\a) ouch, it would be nice if this worked
(set-box! c (ann #\a #\a))