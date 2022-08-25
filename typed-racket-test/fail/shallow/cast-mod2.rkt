#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang typed/racket/base/shallow

(require typed/rackunit)

(check-not-exn
  (lambda ()
    (cast (lambda () 3) (-> String))))

((cast (lambda () 3) (-> String)))

