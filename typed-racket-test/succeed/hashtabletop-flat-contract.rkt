#lang typed/racket/base

;; Test that `HashTableTop` generates a flat contract

(define h : HashTableTop (hash))
(void (cast h HashTableTop))
