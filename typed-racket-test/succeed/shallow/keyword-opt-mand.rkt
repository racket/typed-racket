#lang typed/racket/base/shallow

;; No error when function accepts optional kws
;;  and type says mandatory

(module u racket/base
  (define (f1 x #:y [y #f])
    (void))
  (provide f1))

(require/typed 'u
  (f1 (-> Symbol #:y Symbol Void)))

(void f1)
