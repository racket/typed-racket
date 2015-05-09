#lang typed/racket

(send
  (new
    (class object%
      (super-new)
      (define-values (a b)
        (let ([z 1]) (values z (add1 z))))
      (define c 3)
      (define/public (get-abc) (list a b c))))
  get-abc)
