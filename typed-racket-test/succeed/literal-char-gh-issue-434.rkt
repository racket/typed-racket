#lang typed/racket
;; The precise type for chars should be the char itself:
(ann #\nul #\nul)
(ann #\a #\a)
(ann #\b '#\b)
;; Up-casting as Char should still work:
(ann #\nul Char)
(ann #\a Char)
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