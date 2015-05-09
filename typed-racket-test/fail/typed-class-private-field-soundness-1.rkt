#lang typed/racket

(send
  (new
    (class object%
      (super-new)
      (: a String)
      (define-values (a b) (values 1 2))
      (: get-a (-> String))
      (define/public (get-a) a)))
  get-a)
