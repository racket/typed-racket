#lang typed/racket/base/shallow

;; Test 'with-input-from-file'

(: main (-> Path-String Void))
(define (main filename)
  (define q (with-input-from-file filename (lambda () (read))))
  (void))
