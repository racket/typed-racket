#lang racket/base



(require
  "../../utils/utils.rkt"
  "../structures.rkt"
  "../constraints.rkt"
  racket/match
  racket/generic
  (for-template racket/base)
  (contract-req))

(provide-for-cond-contract symbolic-object-contract?)

(struct symbolic-object-contract static-contract () #:transparent)

;; an identifier symbolic object e.g. x
(struct id/sc symbolic-object-contract (syntax)
  #:transparent
  #:methods gen:equal+hash
  [(define/match (equal-proc a b rec)
     [((id/sc id1) (id/sc id2) rec)
      (free-identifier=? id1 id2)])
   (define/match (hc sc hash-code)
     [((id/sc id) rec)
      (hash-code (identifier-binding-symbol id))])
   (define hash-proc hc)
   (define hash2-proc hc)]
  #:methods gen:sc
  [(define (sc-map v f) v)
   (define (sc-traverse v f) (void))
   (define (sc->contract v f) (id/sc-syntax v))
   (define (sc->constraints v f) (simple-contract-restrict 'flat))])

;; a path element access into a symbolic object e.g. (car o)
(struct acc-obj/sc symbolic-object-contract (acc-stx obj)
  #:transparent
  #:methods gen:equal+hash
  [(define/match (equal-proc a b recur)
     [((acc-obj/sc acc1 obj1) (acc-obj/sc acc2 obj2) rec)
      (and (free-identifier=? acc1 acc2)
           (rec obj1 obj2))])
   (define/match (hc sc hash-code)
     [((acc-obj/sc acc obj) rec)
      (bitwise-ior (rec (identifier-binding-symbol (acc-obj/sc-acc-stx sc)))
                   (rec (acc-obj/sc-obj sc)))])
   (define hash-proc hc)
   (define hash2-proc hc)]
  #:methods gen:sc
  [(define (sc-map v f) (acc-obj/sc (acc-obj/sc-acc-stx v)
                                    (f (acc-obj/sc-obj v) 'covariant)))
   (define (sc-traverse v f) (f (acc-obj/sc-obj v) 'covariant))
   (define/generic sc->c sc->contract)
   (define (sc->contract v f) #`(#,(acc-obj/sc-acc-stx v) #,(sc->c (acc-obj/sc-obj v) f)))
   (define (sc->constraints v f) (f (acc-obj/sc-obj v)))])


;; a linear expression symbolic obj, e.g. 42, or x, or (+ 1 (* 2 y)), etc...
(struct linear-exp/sc symbolic-object-contract (const terms)
  #:transparent
  #:methods gen:sc
  [(define/match (sc-map v f)
     [((linear-exp/sc c ts) f)
      (linear-exp/sc c (for/hash ([(o coeff) (in-hash ts)])
                         (values (f o 'covariant) coeff)))])
   (define/match (sc-traverse v f)
     [((linear-exp/sc c ts) f)
      (for ([o (in-hash-keys ts)])
        (f o 'covariant))])
   (define/generic sc->c sc->contract)
   (define/match (sc->contract v f)
     [((linear-exp/sc c ts) f)
      (define terms-list (for/list ([(o coeff) (in-hash ts)])
                           (if (= 1 coeff)
                               (sc->c o f)
                               #`(* #,coeff #,(sc->c o f)))))
      (cond
        [(null? terms-list) #`#,c]
        [else
         (match terms-list
           [(list) #`#,c]
           [(list t) #:when (zero? c) t]
           [ts #`(+ #,c #,@ts)])])])
   (define/match (sc->constraints v f)
     [((linear-exp/sc c ts) f)
      (merge-restricts* 'flat (for/list ([k (in-hash-keys ts)])
                                (f k)))])])



(provide/cond-contract
 [struct id/sc ([syntax identifier?])]
 [struct acc-obj/sc ([acc-stx identifier?] [obj static-contract?])]
 [struct linear-exp/sc ([const exact-integer?] [terms (hash/c static-contract? exact-integer?)])])
