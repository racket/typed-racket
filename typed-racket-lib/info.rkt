#lang info

(define collection 'multi)

(define deps '(("base" #:version "6.2.0.2")
               "pconvert-lib"
               "unstable-contract-lib"
               "unstable-list-lib"
               "source-syntax"
               "compatibility-lib" ;; to assign types
               "string-constants-lib"))

;; This is needed since the expansion of TR
;; can insert `(require unstable/contract)` into
;; the expanded code.
(define implies '("unstable-contract-lib"))

(define pkg-desc "implementation (no documentation) part of \"typed-racket\"")

(define pkg-authors '(samth stamourv))

(define version "1.1")
