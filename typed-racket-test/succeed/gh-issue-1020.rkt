#lang typed/racket

(class object% (super-new)
  (public m)
  (define m (let ([mm (lambda ([x : Number] #:yyy [yyy : String]) (string-length yyy))])
              mm)))
