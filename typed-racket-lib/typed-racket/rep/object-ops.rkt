#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/lazy-require
         (except-in racket/contract one-of/c)
         ;(prefix-in c: (contract-req))
         (rep object-rep rep-utils filter-rep))

(provide -obj+
         -obj*)

;; cannot lazy require since it's not a function =(
(define -empty-obj (make-Empty))

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
     (LExp-multiply (-lexp (list 0 (list 1 o1)))
                     (-lexp (list 0 (list 1 o2))))]
    [(list-no-order (? LExp? l) (? Path? p))
     (LExp-multiply l (-lexp (list 0 (list 1 p))))]
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
     (-lexp (list 1 o1) (list 1 o2))]
    [(list-no-order (? LExp? l) (? Path? p))
     (LExp-plus l (-lexp (list 1) (list p)))]
    [(list (? LExp?) (? LExp?))
     (LExp-plus o1 o2)]))
