#lang info

(define collection 'multi)

(define build-deps '("net-doc"
                     "net-cookies-doc"
                     "scheme-lib"
                     "srfi-lite-lib"
                     "r6rs-doc"
                     "srfi-doc"
                     "r6rs-lib"
                     "sandbox-lib"
                     "at-exp-lib"
                     ("scribble-lib" #:version "1.16")
                     "pict-lib"
                     ("typed-racket-lib" #:version "1.10")
                     "typed-racket-compatibility"
                     ("typed-racket-more" #:version "1.10")
                     "racket-doc"
                     "draw-lib"
                     "web-server-doc"))
(define deps '("base"))
(define update-implies '("typed-racket-lib"))

(define pkg-desc "documentation part of \"typed-racket\"")

(define pkg-authors '(samth stamourv))

(define version "1.10")
