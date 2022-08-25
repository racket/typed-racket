#lang typed/racket/base/shallow

;; Make sure shallow checks happen within
;;  a test-case form.

(module u racket/base
  (define strs (cons "A" "B"))
  (provide strs))

(require/typed 'u
  (strs (Pairof Symbol Symbol)))
(require typed/rackunit)

(test-case "a"
  (check-exn exn:fail:contract?
    (lambda () (car strs))))
