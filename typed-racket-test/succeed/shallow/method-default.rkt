#lang typed/racket/shallow

;; Test immediate and expression defaults
;;  for optional & keyword arguments


(let ()
  ;; optional, value
  (: c1% (Class (f (->* [Integer] [Integer] Integer))))
  (define c1%
    (class object%
      (super-new)
      (define/public (f x [y 0]) (+ 4 4))))
  (send (new c1%) f 0))

(let ()
  ;; optional, expression
  (: c1% (Class (f (->* [Integer] [Integer] Integer))))
  (define c1%
    (class object%
      (super-new)
      (define/public (f x [y (+ 0 1)]) (+ 4 4))))
  (send (new c1%) f 0))

(let ()
  ;; optional, field
  (: c1% (Class (field (a Integer)) (f (->* [Integer] [Integer] Integer))))
  (define c1%
    (class object%
      (super-new)
      (field (a 42))
      (define/public (f x [y a]) (+ 4 4))))
  (send (new c1%) f 0))

(let ()
  ;; keyword, value
  (: c1% (Class (f (->* [Integer] [#:y Integer] Integer))))
  (define c1%
    (class object%
      (super-new)
      (define/public (f x #:y [y 0]) (+ 4 4))))
  (send (new c1%) f 0))

(let ()
  ;; keyword, expression
  (: c1% (Class (f (->* [Integer] [#:y Integer] Integer))))
  (define c1%
    (class object%
      (super-new)
      (define/public (f x #:y [y (+ 0 1)]) (+ 4 4))))
  (send (new c1%) f 0))

(let ()
  ;; keyword, field
  (: c1% (Class (field (a Integer)) (f (->* [Integer] [#:y Integer] Integer))))
  (define c1%
    (class object%
      (super-new)
      (field (a 42))
      (define/public (f x #:y [y a]) (+ 4 4))))
  (send (new c1%) f 0))

