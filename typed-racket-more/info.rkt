#lang info

(define collection 'multi)

(define deps '("srfi-lite-lib"
               "base"
	       "net-lib"
               "net-cookies-lib"
	       ["web-server-lib" #:version "1.6"]
               ["db-lib" #:version "1.5"]
               "draw-lib"
               "rackunit-lib"
               "rackunit-gui"
               "rackunit-typed"
               "snip-lib"
               ["typed-racket-lib" #:version "1.14"]
               ["gui-lib" #:version "1.49"]
               "pict-lib"
               "images-lib"
               "racket-index"
               "sandbox-lib"
               "pconvert-lib"))

(define implies '("rackunit-typed"))

(define pkg-desc "Types for various libraries")

(define pkg-authors '(samth stamourv))

(define version "1.14")
