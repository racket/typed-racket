#lang racket/base

(module constr-name typed/racket/base
  (provide (type-out
    (struct s () #:constructor-name makes #:omit-constructor))))
(require 'constr-name)
makes
