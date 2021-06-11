#lang typed/racket/base

(require racket/port)

(define-type Foo (-> (U Exact-Positive-Integer False)
                     (U Exact-Nonnegative-Integer False)
                     (U Exact-Positive-Integer False)
                     (U Exact-Nonnegative-Integer False)
                     Any))

(: make-validating-input-port (-> Input-Port Input-Port))
(define (make-validating-input-port in)
  (: read-wrap (-> Bytes
                   (U Foo
                      (Evtof Any)
                      EOF
                      Exact-Nonnegative-Integer)
                   EOF))
  (define (read-wrap bstr whatever)
    eof)
  (: peek-wrap (-> Bytes
                   Exact-Nonnegative-Integer
                   (Option (Evtof Exact-Nonnegative-Integer))
                   (Option
                    (U Foo
                       (Evtof Any)
                       EOF
                       Exact-Nonnegative-Integer))
                   EOF))
  (define (peek-wrap bstr skip evt whatever)
    eof)
  (filter-read-input-port
   in
   read-wrap
   peek-wrap))
