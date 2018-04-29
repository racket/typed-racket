#lang typed/racket/shallow

;; Defender should work for 'send' calls in a dead lambda branch

(define-type C% (Class [f (-> Real Real)]))

(: c% C%)
(define c%
  (class object%
    (super-new)
    (define/public (f x)
      (+ x x))))

(if #true
  (void)
  (void (send (new c%) f 42)))
