#;
(exn-pred exn:fail:contract? #rx"string-length: contract violation")
#lang racket
(module test racket/base
  (string-length 1))
