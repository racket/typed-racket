#lang racket/base

;; A test for PR 15026, goes with pr15026-a.rkt
;;
;; Should not segfault

(module+ test
  (require (except-in "pr15026-a.rkt" x))
  y)
