#lang info

(define collection 'multi)

(define deps '(("base" #:version "6.3.0.8")
               "pconvert-lib"
               "source-syntax"
               "compatibility-lib" ;; to assign types
               "string-constants-lib"))

(define pkg-desc "implementation (no documentation) part of \"typed-racket\"")

(define pkg-authors '(samth stamourv))

(define version "1.3")
