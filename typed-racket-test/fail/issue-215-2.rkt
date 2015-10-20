#lang typed/racket


(ann (let ((x 'b))
       (if (equal? x 'a)
           42
           "foo")) Number)
