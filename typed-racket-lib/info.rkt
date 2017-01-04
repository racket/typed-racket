#lang info

(define collection 'multi)

(define deps '(("base" #:version "6.7.0.4")
               "source-syntax"
               "compatibility-lib" ;; to assign types
               "string-constants-lib"))

(define pkg-desc "implementation (no documentation) part of \"typed-racket\"")

(define pkg-authors '(samth stamourv))

(define version "1.6")
