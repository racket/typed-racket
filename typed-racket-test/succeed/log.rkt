#lang typed/racket/base

(make-log-receiver (make-logger) 'error)
(make-log-receiver (make-logger) 'error 'hi)

;; the expr should type check
;; (make-log-receiver (make-logger) 'error 'hi 'fatal)

(make-log-receiver (make-logger) 'error 'hi 'fatal 'world)
