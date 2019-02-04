#lang info

(define collection 'multi)

(define deps '("srfi-lite-lib"
               "base"
	       "net-lib"
	       "web-server-lib"
               ["db-lib" #:version "1.5"]
               "draw-lib"
               "rackunit-lib"
               "rackunit-gui"
               "rackunit-typed"
               "snip-lib"
               "typed-racket-lib"
               "gui-lib"
               "pict-lib"
               "images-lib"
               "racket-index"
               "sandbox-lib"))

(define implies '("rackunit-typed"))

(define pkg-desc "Types for various libraries")

(define pkg-authors '(samth stamourv))

(define version "1.9")
