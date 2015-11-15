#lang racket/base

(require racket/dict racket/sequence)
(provide id< sorted-dict-map in-sorted-dict)

(define (id< a b) (symbol<? (syntax-e a) (syntax-e b)))

(define (sorted-dict-map dict f <)
  (define sorted (sort #:key car (dict-map dict cons) <))
  (map (lambda (a) (f (car a) (cdr a))) sorted))

(define (in-sorted-dict dict <)
  (define sorted (sort #:key car (dict-map dict cons) <))
  (in-dict sorted))
