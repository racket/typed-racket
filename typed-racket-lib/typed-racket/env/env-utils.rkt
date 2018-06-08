#lang racket/base

(require "../utils/utils.rkt" syntax/private/id-table)
(provide id<
         sorted-free-id-table-map
         sorted-free-id-table-for-each
         in-sorted-free-id-table
         in-sorted-free-id-table-keys)

(define (id< a b) (symbol<? (syntax-e a) (syntax-e b)))

(define-syntax-rule (in-sorted-free-id-table table)
  (in-assoc (sort (for/list ([(k v) (in-free-id-table table)])
                    (cons k v))
                  (λ (entry1 entry2) (id< (car entry1) (car entry2))))))

(define-syntax-rule (in-sorted-free-id-table-keys table)
  (in-list (sort (for/list ([(k _) (in-free-id-table table)])
                   k)
                 id<)))

(define (sorted-free-id-table-map table f)
  (for/list ([(k v) (in-sorted-free-id-table table)])
    (f k v)))

(define (sorted-free-id-table-for-each table f)
  (for ([(k v) (in-sorted-free-id-table table)])
    (f k v)))
