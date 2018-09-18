#lang racket/base

;; Unit tests for typed/json

(require typed/json
         (only-in json json-null)
         rackunit)

(provide tests)

(define tests
  (test-suite
   "Tests for typed/json"
   (test-case
    "test that json-null parameter can't break typed/json"
    (parameterize ([json-null '|bogus json-null|])
      ;; predicate
      (check-false (jsexpr? '|bogus json-null|))
      (check-true (jsexpr? 'null))
      ;; writing
      (check-equal? (let ([out (open-output-string)])
                      (write-json 'null out)
                      (get-output-string out))
                    "null")
      (check-equal? (jsexpr->string 'null)
                    "null")
      (check-equal? (jsexpr->bytes 'null)
                    #"null")
      ;; reading
      (check-eq? (read-json (open-input-string "null"))
                 'null)
      (check-eq? (string->jsexpr "null")
                 'null)
      (check-eq? (bytes->jsexpr #"null")
                 'null)))))

