#lang racket

;;; https://github.com/racket/typed-racket/pull/467

;;; due to "instantiate the gui instance one more time" reported by travis-ci.
;;; This test cheats the continuous integration environment.
;;; Actually it will typecheck but won't run.

(module cheat-foo typed/racket/gui/no-check
  (: f (Integer -> Any))
  (define (f x) (add1 (current-eventspace)))
 
  (lambda ([x : String]) (string-append " " x)))
