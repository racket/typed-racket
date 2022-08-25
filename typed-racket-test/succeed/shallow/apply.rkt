#lang typed/racket/base/shallow

;; Test that `(apply f ...)`  does not check its result when `f` is a
;;  trusted function

(void (apply string-append '("one" "two" "three")))


;; Apply works when we need protection

(module u racket/base
  (provide f0 f1 f2)
  (define (f0) (values))
  (define (f1) (values 'a))
  (define (f2) (values 'a 'b)))

(require/typed 'u
  (f0 (-> (Values)))
  (f1 (-> Symbol))
  (f2 (-> (Values Symbol Symbol))))

(define-values [] (apply f0 '()))

(: a Symbol)
(define-values [a] (apply f1 '()))

(: b Symbol)
(: c Symbol)
(define-values [b c] (apply f2 '()))


;; One more apply
(call-with-values (lambda () (apply values (ann '(x x x) (Listof Symbol)))) void)
