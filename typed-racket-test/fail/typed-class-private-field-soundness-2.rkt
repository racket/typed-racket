#lang typed/racket

(send
  (new
    (class object%
      (super-new)
      (: a String)
      (define-values (a b) (let ([z 1]) (values z z)))
      (: get-a (-> String))
      (define/public (get-a) a)))
  get-a)
