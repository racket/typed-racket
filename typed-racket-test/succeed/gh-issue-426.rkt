#lang typed/racket

;; Test for Github issue #426
;; https://github.com/racket/typed-racket/issues/426

(require racket/string typed/rackunit)

(check-equal? (non-empty-string? "foo")            #true)
(check-equal? (non-empty-string? "")               #false)
(check-equal? (if (non-empty-string? "foo") 'A 'B) 'A)
(check-equal? (if (non-empty-string? "")    'A 'B) 'B)

