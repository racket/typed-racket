#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/lazy-require
         (except-in racket/contract one-of/c)
         ;(prefix-in c: (contract-req))
         (rep object-rep rep-utils filter-rep)
         fme)

(provide -obj+
         -obj*
         LExp-map
         constant-LExp?)

;; cannot lazy require since it's not a function =(
(define -empty-obj (make-Empty))

;;******************************************************************************
;; LExp Functions

;; applies f to each Path p in l
;; for any Path p in l, if (f p) = Empty 
;; then Empty is returned
(define/cond-contract (LExp-map f l)
  (-> (-> Path? Object?) LExp? Object?)
  (match l
    [(LExp: ps)
     (define contains-empty? #f)
     (define c (LExp-const l))
     (define-values (cs ps*) 
       (for/lists (cs xs)
         ([x (in-list ps)])
         (let ([x* (f x)])
           (when (Empty? x*)
             (set! contains-empty? #t))
           (values (LExp-coeff l x) x*))))
     (if contains-empty?
         -empty-obj
         (make-LExp c cs ps*))]))

(define/cond-contract (constant-LExp? l)
  (-> LExp? (or/c #f exact-integer?))
  (cond
    [(null? (LExp-paths l)) (LExp-const l)]
    [else #f]))


;;******************************************************************************
;; Mathematical operations for Objects (potentially producing LExps)
(define/cond-contract (-obj* objs)
  (-> (listof Object?) (or/c Object? #f))
  (match objs
    [(list) #f]
    [(list o) o]
    [(list o1 o2) (multiply-Objects o1 o2)]
    [(list o1 o2 o3 os ...)
     (-obj* (cons (multiply-Objects o1 o2) (cons o3 os)))]))

(define (multiply-Objects o1 o2)
  (match (list o1 o2)
    [(list-no-order (? Empty? o) _) o]
    [(list (? Path?) (? Path?))
     (LExp-multiply (make-LExp 0 (list 1) (list o1))
                     (make-LExp 0 (list 1) (list o2)))]
    [(list-no-order (? LExp? l) (? Path? p))
     (LExp-multiply l (make-LExp 0 (list 1) (list p)))]
    [(list (? LExp?) (? LExp?))
     (LExp-multiply o1 o2)]))


(define/cond-contract (-obj+ objs)
  (-> (listof Object?) (or/c Object? #f))
  (match objs
    [(list) #f]
    [(list o) o]
    [(list o1 o2) (add-Objects o1 o2)]
    [(list o1 o2 o3 os ...)
     (-obj+ (cons (add-Objects o1 o2) (cons o3 os)))]))

(define (add-Objects o1 o2)
  (match (list o1 o2)
    [(list-no-order (? Empty? o) _) o]
    [(list (? Path?) (? Path?))
     (make-LExp 0 (list 1 1) (list o1 o2))]
    [(list-no-order (? LExp? l) (? Path? p))
     (LExp-add l (make-LExp 0 (list 1) (list p)))]
    [(list (? LExp?) (? LExp?))
     (LExp-add o1 o2)]))
