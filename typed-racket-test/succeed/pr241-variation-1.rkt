#lang racket/base

;; #:opaque predicates should not change the type of their arguments

(module untyped racket/base
 (define (bad x)
   (set-box! x 5)
   #t)
 (provide bad))

(module typed typed/racket/base
 (require/typed (submod ".." untyped)
   [#:opaque T bad])
 (: b (Boxof String))
 (define b (box "hi"))
 (with-handlers ([exn:fail:contract? (lambda (e) (void))])
   (bad b)
   (void))
 (string-append (unbox b) ""))

(require 'typed)
