#lang typed/racket/base
(require/typed file/sha1
  [sha1 (->* [(U Bytes Input-Port)]
             [Exact-Nonnegative-Integer
              (U Exact-Nonnegative-Integer False)]
             String)]
  [sha1-bytes (->* [(U Bytes Input-Port)]
                   [Exact-Nonnegative-Integer
                    (U Exact-Nonnegative-Integer False)]
                   Bytes)]
  [bytes->hex-string (-> Bytes String)]
  [hex-string->bytes (-> String Bytes)])
(provide sha1 sha1-bytes bytes->hex-string hex-string->bytes)
