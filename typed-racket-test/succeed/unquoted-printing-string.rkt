#lang typed/racket

(define ups : Unquoted-Printing-String
  (unquoted-printing-string "hello world"))

(: unwrap (-> (U String Unquoted-Printing-String) String))
(define (unwrap v)
  (if (unquoted-printing-string? v)
      (unquoted-printing-string-value v)
      v))

(let ([got (unwrap ups)])
  (unless (equal? got "hello world")
    (error 'unquoted-printing-string "bad result: ~e" got)))
