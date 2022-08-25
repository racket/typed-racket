#lang typed/racket/base/shallow

(: my-param (Parameterof Symbol))
(define my-param (make-parameter 'A))

(my-param 'B) ;; no shape-check
(my-param) ;; yes shape-check

