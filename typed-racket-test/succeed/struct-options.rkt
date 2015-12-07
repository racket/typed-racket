#lang typed/racket/base

;; Tests for constructor options for struct

(struct s1 ([x : Integer]) #:constructor-name cons-s1)
(define-struct s2 ([x : Integer]) #:constructor-name cons-s2)
(struct s3 ([x : Integer]) #:extra-constructor-name cons-s3)
(define-struct s4 ([x : Integer]) #:extra-constructor-name cons-s4)

(cons-s1 1)
(cons-s2 2)
(s3 3)
(cons-s3 3)
(s4 4)
(cons-s4 4)
