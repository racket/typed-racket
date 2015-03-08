#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/lazy-require
         (except-in racket/contract one-of/c)
         ;(prefix-in c: (contract-req))
         (rep object-rep rep-utils)
         fme)

(provide -obj+
         -obj*)

;; cannot lazy require since it's not a function =(
(define -empty-obj (make-Empty))

;;******************************************************************************
;; Mathematical operations for Objects
(define (-obj* objs)
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
     (multiply-LExps (make-LExp 0 (list 1) (list o1))
                     (make-LExp 0 (list 1) (list o2)))]
    [(list-no-order (? LExp? l) (? Path? p))
     (multiply-LExps l (make-LExp 0 (list 1) (list p)))]
    [(list (? LExp?) (? LExp?))
     (multiply-LExps o1 o2)]))

(define/cond-contract (scale-LExp l c)
  (-> LExp? exact-integer? LExp?)
  (match-let ([(LExp: e ps) l])
    (let ([e* (lexp-scale e c)])
      (if (lexp-zero? e*)
          (make-LExp e* null)
          (make-LExp e* ps)))))

(define/cond-contract (multiply-LExps l1 l2)
  (-> LExp? LExp? Object?)
  (cond
    [(lexp-constant? l1) 
     => (λ (const) (scale-LExp l2 const))]
    [(lexp-constant? l2) 
     => (λ (const) (scale-LExp l1 const))]
    [else -empty-obj]))


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
     (add-LExps l (make-LExp 0 (list 1) (list p)))]
    [(list (? LExp?) (? LExp?))
     (add-LExps o1 o2)]))

(define (add-LExps l1 l2)
  (-> LExp? LExp? (or/c LExp? Path?))
  (match-let ([(LExp: e1 ps1) l1]
              [(LExp: e2 ps2) l2])
    (let* ([e* (lexp-plus e1 e2)]
           [ps1* (filter (λ (p) (not (zero? (lexp-coeff e* (Rep-seq p))))) 
                         ps1)]
           [ps2* (filter (λ (p) (not (zero? (lexp-coeff e* (Rep-seq p))))) 
                         ps2)]
           [ps* (union-Path-lists ps1* ps2*)])
      (cond
        ;; if the linear expression is equivalent to just a Path, then just
        ;; return that Path
        [(and (= 1 (length ps*))
              (= 1 (lexp-coeff e* (Rep-seq (car ps*))))
              (zero? (lexp-const e*)))
         (car ps*)]
        [else (make-LExp e* ps*)]))))


(define (union-Path-lists ps1 ps2)
  (match ps1
    [(list) ps2]
    [(cons p ps)
     (let ([p-seq (Rep-seq p)])
       (cond
         [(for/or ([p* (in-list ps2)]) 
            (equal? p-seq (Rep-seq p*)))
          (union-Path-lists ps ps2)]
         [else
          (union-Path-lists ps (cons p ps2))]))]))