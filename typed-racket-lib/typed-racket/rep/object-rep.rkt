#lang racket/base

;; Representation of "objects" --- these describe the
;; part of an environment that an expression accesses
;;
;; See "Logical Types for Untyped Languages" pg.3

(require "rep-utils.rkt" 
         "free-variance.rkt" 
         "filter-rep.rkt" 
         "../utils/utils.rkt" 
         racket/match
         (contract-req)
         fme)

(provide object-equal?
         LExp?
         LExp:
         (rename-out [LExp* make-LExp]))

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

(def-object Path ([p (listof PathElem?)] [v name-ref/c])
  [#:intern (list (map Rep-seq p) (hash-name v))]
  [#:frees (λ (f) (combine-frees (map f p)))]
  [#:fold-rhs (*Path (map pathelem-rec-id p) v)])

(define (LExp* const coeffs paths)
  (let ([coeffs/paths (for/list ([c (in-list coeffs)]
                                 [p (in-list paths)])
                        (list c (Rep-seq p)))])
    (*LExp (list->lexp (cons const coeffs/paths))
           paths)))

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

