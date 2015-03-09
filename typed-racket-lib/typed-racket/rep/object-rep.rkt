#lang racket/base

;; Representation of "objects" --- these describe the
;; part of an environment that an expression accesses
;;
;; See "Logical Types for Untyped Languages" pg.3

(require "rep-utils.rkt" 
         "free-variance.rkt" 
         "filter-rep.rkt" 
         "../utils/utils.rkt" 
         (except-in racket/contract one-of/c)
         racket/match
         (contract-req)
         fme
         (for-syntax racket/base syntax/parse))

(provide object-equal?
         LExp?
         LExp-coeff
         LExp-const
         LExp-paths
         LExp-multiply
         LExp-add
         (rename-out [LExp:* LExp:]
                     [LExp* make-LExp]))

(def-pathelem CarPE () [#:fold-rhs #:base])
(def-pathelem CdrPE () [#:fold-rhs #:base])
(def-pathelem SyntaxPE () [#:fold-rhs #:base])
(def-pathelem ForcePE () [#:fold-rhs #:base])
;; t is always a Name (can't put that into the contract b/c of circularity)
(def-pathelem StructPE ([t Type?] [idx natural-number/c])
  [#:frees (λ (f) (f t))]
  [#:fold-rhs (*StructPE (type-rec-id t) idx)])
(def-pathelem LengthPE () [#:fold-rhs #:base])

(def-object Empty () [#:fold-rhs #:base])
(define -empty-obj (*Empty))
(def-object Path ([p (listof PathElem?)] [v name-ref/c])
  [#:intern (list (map Rep-seq p) (hash-name v))]
  [#:frees (λ (f) (combine-frees (map f p)))]
  [#:fold-rhs (*Path (map pathelem-rec-id p) v)])

(def-object LExp ([exp lexp?] [paths (listof Path?)])
  #:no-provide
  [#:intern exp]
  [#:frees (λ (f) (combine-frees (map f paths)))]
  [#:fold-rhs (let ([c (lexp-const exp)]
                    [paths* (map object-rec-id paths)]
                    [coeffs (map (λ (p) (lexp-coeff exp p)) paths)])
                (LExp* c coeffs paths*))])


;; represents no info about the object of this expression
;; should only be used for parsing type annotations and expected types
(def-object NoObject () [#:fold-rhs #:base])

(define (object-equal? o1 o2) (= (Rep-seq o1) (Rep-seq o2)))

#|
(dlo LEmpty () [#:fold-rhs #:base])

(dlo LPath ([p (listof PathElem?)] [idx index/c])
  [#:frees (λ (f) (combine-frees (map f p)))]
  [#:fold-rhs (*LPath (map pathelem-rec-id p) idx)])
|#

;; LExp ops
;;******************************************************************************
;; LExp functions that need access to raw lexp? 
;; (the outside world only can access the paths
;;  and use functions defined here)
(define (LExp* const coeffs paths)
  (let ([coeffs/paths (for/list ([c (in-list coeffs)]
                                 [p (in-list paths)])
                        (list c (Rep-seq p)))])
    (*LExp (list->lexp (cons const coeffs/paths))
           paths)))

(define/cond-contract (LExp-coeff l p)
  (-> LExp? Path? exact-integer?)
  (match l
    [(LExp: e _) (lexp-coeff e (Rep-seq p))]))

(define/cond-contract (LExp-const l)
  (-> LExp? exact-integer?)
  (match l
    [(LExp: e _) (lexp-const e)]))

(define-match-expander LExp:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ ps) #'(? LExp? (app LExp-paths ps))])))

(define/cond-contract (scale-LExp l c)
  (-> LExp? exact-integer? LExp?)
  (match-let ([(LExp: e ps) l])
    (let ([e* (lexp-scale e c)])
      (if (lexp-zero? e*)
          (make-LExp e* null)
          (make-LExp e* ps)))))

(define/cond-contract (LExp-multiply l1 l2)
  (-> LExp? LExp? Object?)
  (cond
    [(lexp-constant? l1) 
     => (λ (const) (scale-LExp l2 const))]
    [(lexp-constant? l2) 
     => (λ (const) (scale-LExp l1 const))]
    [else -empty-obj]))

(define (LExp-add l1 l2)
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

