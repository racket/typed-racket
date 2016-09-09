#lang racket/base

(require racket/dict racket/sequence)
(provide id< sorted-dict-map sorted-dict-for-each in-sorted-dict)

(define (id< a b) (symbol<? (syntax-e a) (syntax-e b)))

(define (sorted-dict-map dict f <)
  (define sorted (sort (dict-map dict cons) (λ (x y) (< (car x) (car y)))))
  (map (lambda (a) (f (car a) (cdr a))) sorted))

(define (sorted-dict-for-each dict f <)
  (define sorted (sort (dict-map dict cons) (λ (x y) (< (car x) (car y)))))
  (for-each (lambda (a) (f (car a) (cdr a))) sorted))

(define (in-sorted-dict dict <)
  (define sorted (sort #:key car (dict-map dict cons) <))
  (in-dict sorted))
