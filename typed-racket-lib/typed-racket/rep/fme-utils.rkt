#lang racket/base

(require "../utils/utils.rkt"
         "core-rep.rkt"
         (for-syntax racket/base)
         (contract-req))

(provide (all-defined-out))


(define-for-cond-contract terms/c (hash/c Object? (and/c exact-integer?
                                                         (not zero?))))


(define-syntax terms-count (make-rename-transformer #'hash-count))
(define-syntax terms-vars (make-rename-transformer #'hash-keys))
(define-syntax terms-coeffs (make-rename-transformer #'hash-values))
(define-syntax terms-empty? (make-rename-transformer #'hash-empty?))
(define-syntax in-terms (make-rename-transformer #'in-immutable-hash))
(define-syntax in-terms-vars (make-rename-transformer #'in-immutable-hash-keys))
(define-syntax in-terms-coeffs (make-rename-transformer #'in-immutable-hash-values))
(define-syntax terms-remove (make-rename-transformer #'hash-remove))
(define-syntax make-terms (make-rename-transformer #'hasheq))
(define-syntax terms->list (make-rename-transformer #'hash->list))

(define-syntax-rule (terms-ref ts x)
  (hash-ref ts x 0))

(define/cond-contract (terms-set h x i)
  (-> (hash/c any/c exact-integer? #:immutable #t)
      any/c
      exact-integer?
      (hash/c any/c exact-integer? #:immutable #t))
  (if (= 0 i)
      (hash-remove h x)
      (hash-set h x i)))

(define/cond-contract (terms-scale ts a)
  (-> (and/c hash? hash-eq?) rational? (and/c hash? hash-eq?))
  (cond
    [(zero? a) (hasheq)]
    [(= a 1) ts]
    [else
     (for/hasheq ([(x coeff) (in-hash ts)])
       (values x (* coeff a)))]))

(define/cond-contract (terms-subtract l1-terms l2-terms)
  (-> (and/c hash? hash-eq?) (and/c hash? hash-eq?) (and/c hash? hash-eq?))
  (for/fold ([h l1-terms])
            ([(x coeff) (in-hash l2-terms)])
    (terms-set h x (- (hash-ref l1-terms x 0) coeff))))

(define/cond-contract (terms-add l1-terms l2-terms)
  (-> (and/c hash? hash-eq?) (and/c hash? hash-eq?) (and/c hash? hash-eq?))
  (for/fold ([h l1-terms])
            ([(x coeff) (in-hash l2-terms)])
    (terms-set h x (+ (hash-ref l1-terms x 0) coeff))))