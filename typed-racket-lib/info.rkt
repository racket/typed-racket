#lang info

(define collection 'multi)

(define deps '(("base" #:version "8.12.0.14")
               "source-syntax"
               "pconvert-lib"
               "compatibility-lib" ;; to assign types
               "string-constants-lib"))

(define pkg-desc "implementation (no documentation) part of \"typed-racket\"")

(define pkg-authors '(samth stamourv))

(define version "1.15")

(define license
  '(Apache-2.0 OR MIT))
