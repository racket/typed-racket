#lang racket/base

;; Static contracts for structural contracts.
;; Ex: list/sc, vectorof/sc

(require "../../utils/utils.rkt"
         "../structures.rkt"
         "../constraints.rkt"
         racket/match
         (for-syntax racket/base racket/syntax syntax/stx syntax/parse)
         racket/set
         racket/sequence
         (for-template racket/base
                       racket/contract/base
                       racket/set
                       racket/async-channel
                       racket/sequence
                       racket/promise
                       "../../utils/evt-contract.rkt"
                       "../../utils/hash-contract.rkt"
                       "../../utils/vector-contract.rkt"
                       "../../utils/promise-not-name-contract.rkt")
         racket/contract
         racket/async-channel)


(begin-for-syntax
  (define-syntax-class variance-keyword
    #:attributes (variance)
    [pattern (~and kw (~or #:covariant #:contravariant #:invariant))
             #:with variance (string->symbol (keyword->string (syntax-e (attribute kw))))])

  (define-syntax-class contract-category-keyword
    #:attributes (category category-stx)
    [pattern (~and kw (~or #:flat #:chaperone #:impersonator))
             #:attr category (string->symbol (keyword->string (syntax-e (attribute kw))))
             #:with category-stx (attribute category)])

  ;; TODO: Fix category when syntax parse is fixed
  (define-syntax-class argument-description
    #:attributes (variance name category category-stx)
    [pattern ((~or (~optional c:contract-category-keyword)
                   (~once :variance-keyword)) ...)
             #:attr name (generate-temporary)
             #:attr category (or (attribute c.category) 'impersonator)
             #:with category-stx (attribute category)])

  (define-syntax-class static-combinator-form
    #:attributes (name struct-name definition combinator2 ->restricts matcher provides map traverse)
    [pattern (name:id pos:argument-description ... )
             #:with struct-name (generate-temporary #'name)
             #:with matcher-name (format-id #'name "~a:" #'name)
             #:with definition
               #'(define name (λ (pos.name ...) (struct-name (list pos.name ...))))
             #:with ->restricts
               #'(lambda (v recur)
                   (for/list ([arg (in-list (combinator-args v))]
                              [kind (in-list (list 'pos.category-stx ...))])
                     (add-constraint (recur arg) kind)))
             #:attr combinator2
               #'(λ (constructor) (λ (pos.name ...) (constructor (list pos.name ...))))
             #:with matcher
               #'(define-match-expander matcher-name
                   (syntax-parser
                     [(_ pos.name ...)
                      #'(struct-name (list pos.name ...))]))
             #:with map
               #'(lambda (v f)
                   (struct-name
                     (for/list ([a (in-list (combinator-args v))]
                                [kind (in-list (list 'pos.variance ...))])
                       (f a kind))))
             #:with traverse
               #'(lambda (v f)
                   (for ([a (in-list (combinator-args v))]
                         [kind (in-list (list 'pos.variance ...))])
                     (f a kind)))
             #:with ctc
                 #`(-> #,@(stx-map (lambda (_) #'static-contract?) #'(pos ...)) static-contract?)
             #:with provides #'(begin (provide matcher-name)
                                      (provide/cond-contract [name ctc]))]
    [pattern (name:id . rest:argument-description)
             #:with struct-name (generate-temporary #'name)
             #:with matcher-name (format-id #'name "~a:" #'name)
             #:with definition #'(define name (λ args (struct-name args)))
             #:attr combinator2 #'(λ (constructor) (λ args (constructor args)))
             #:with ->restricts
               #'(lambda (v recur)
                   (for/list ([arg (in-list (combinator-args v))])
                     (add-constraint (recur arg) 'rest.category-stx)))
             #:with matcher
               #'(define-match-expander matcher-name
                   (syntax-parser
                    [(_ ctc (... ...))
                     #'(struct-name (list ctc (... ...)))]))
             #:with map
               #'(lambda (v f)
                   (struct-name
                     (for/list ([a (in-list (combinator-args v))])
                       (f a 'rest.variance))))
             #:with traverse
               #'(lambda (v f)
                   (for ([a (in-list (combinator-args v))])
                     (f a 'rest.variance)))
             #:with ctc
                 #'(->* () #:rest (listof static-contract?) static-contract?)
             #:with provides #'(begin (provide matcher-name)
                                      (provide/cond-contract [name ctc]))]))


(define-syntax (combinator-struct stx)
  (syntax-parse stx
    [(_ sc:static-combinator-form c:expr kind:contract-category-keyword)
     #'(begin
         (struct sc.struct-name combinator ()
           #:transparent
           #:methods gen:sc
           [(define sc-map sc.map)
            (define sc-traverse sc.traverse)
            (define (sc->contract v recur)
              (apply
               (sc.combinator2 (lambda (args) #`(c #,@args)))
               (map recur (combinator-args v))))
            (define (sc->constraints v recur)
              (merge-restricts* 'kind.category-stx (sc.->restricts v recur)))]
           #:methods gen:equal+hash
           [(define (equal-proc a b recur)
              (and (eqv? (length (combinator-args a))
                         (length (combinator-args b)))
                   (for/and ([sub-a (in-list (combinator-args a))]
                             [sub-b (in-list (combinator-args b))])
                     (recur sub-a sub-b))))
            (define (hash-proc v recur)
              (for/fold ([hc (recur 'sc.name)])
                        ([sub (in-list (combinator-args v))])
                (bitwise-ior hc (recur sub))))
            (define (hash2-proc v recur)
              (for/fold ([hc (recur 'sc.name)])
                        ([sub (in-list (combinator-args v))])
                (bitwise-ior hc (recur sub))))]
           #:property prop:combinator-name (symbol->string 'sc.name))
         sc.matcher
         sc.definition
         sc.provides)]))


(define-syntax (combinator-structs stx)
  (syntax-parse stx
    [(_ (e ...) ...)
     #`(begin
         (combinator-struct e ...) ...)]))

(combinator-structs
  ((or/sc . (#:covariant)) or/c #:flat)
  ((and/sc . (#:covariant)) and/c #:flat)
  ((list/sc . (#:covariant)) list/c #:flat)
  ((listof/sc (#:covariant)) listof #:flat)
  ((cons/sc (#:covariant) (#:covariant)) cons/c #:flat)
  ((set/sc (#:covariant #:chaperone)) set/c #:flat)
  ((immutable-vectorof/sc (#:covariant)) immutable-vectorof/c #:flat)
  ((mutable-vectorof/sc (#:invariant)) mutable-vectorof/c #:chaperone)
  ((vectorof/sc (#:invariant)) vectorof #:chaperone)
  ((immutable-vector/sc . (#:covariant)) immutable-vector/c #:flat)
  ((mutable-vector/sc . (#:invariant)) mutable-vector/c #:chaperone)
  ((vector/sc . (#:invariant)) vector/c #:chaperone)
  ((promise/sc (#:covariant)) promise-not-name/c #:chaperone)
  ((syntax/sc (#:covariant #:flat)) syntax/c #:flat)
  ((hash/sc (#:invariant) (#:invariant)) typed-racket-hash/c #:chaperone)
  ((mutable-hash/sc (#:invariant) (#:invariant)) mutable-hash/c #:chaperone)
  ((immutable-hash/sc (#:covariant) (#:covariant)) immutable-hash/c #:flat)
  ((weak-hash/sc (#:invariant) (#:invariant)) weak-hash/c #:chaperone)
  ((box/sc (#:invariant)) box/c #:chaperone)
  ((parameter/sc (#:contravariant) (#:covariant)) parameter/c #:chaperone)
  ((sequence/sc . (#:covariant)) sequence/c #:impersonator)
  ((channel/sc . (#:invariant)) channel/c #:chaperone)
  ((continuation-mark-key/sc (#:invariant)) continuation-mark-key/c #:chaperone)
  ((evt/sc (#:covariant)) tr:evt/c #:chaperone)
  ((async-channel/sc (#:invariant)) async-channel/c #:chaperone))
