#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang typed/racket/base/shallow

(require/typed racket/base
 (values (-> Any Any : String)))

(define x : Any 0)

(define fake-str : String
  (if (values x)
    (ann x String)
    (error 'unreachable)))

(string-length fake-str)

