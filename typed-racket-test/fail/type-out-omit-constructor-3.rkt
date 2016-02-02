#lang racket/base

(module constr-name typed/racket/base
  (provide (type-out
    (struct s () #:omit-constructor #:constructor-name makes))))
(require 'constr-name)
makes
