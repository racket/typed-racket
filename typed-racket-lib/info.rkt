#lang info

(define collection 'multi)

(define deps '(("base" #:version "7.3.0.11")
               "source-syntax"
               "pconvert-lib"
               "compatibility-lib" ;; to assign types
               "string-constants-lib"))

(define pkg-desc "implementation (no documentation) part of \"typed-racket\"")

(define pkg-authors '(samth stamourv))

(define version "1.10")
