#lang racket/base

;; This file is for the abbreviations needed to implement union.rkt
;;
;; The "abbrev.rkt" module imports this module, re-exports it, and
;; extends it with more types and type abbreviations.

(require "../utils/utils.rkt"
         "../utils/identifier.rkt"
         "../rep/type-rep.rkt"
         "../rep/prop-rep.rkt"
         "../rep/object-rep.rkt"
         "../rep/base-types.rkt"
         "../rep/numeric-base-types.rkt"
         (rep values-rep rep-utils free-ids)
         (env mvar-env)
         racket/match racket/list (prefix-in c: (contract-req))
         (for-syntax racket/base syntax/parse racket/list)
         ;; For contract predicates
         (for-template racket/base))

(provide (all-defined-out)
         (all-from-out "../rep/type-rep.rkt"
                       "../rep/object-rep.rkt"
                       "../rep/base-types.rkt"
                       "../rep/prop-rep.rkt"
                       "../rep/numeric-base-types.rkt")
         (rename-out [make-Listof -lst]
                     [make-MListof -mlst]))

(define-syntax (rebuild stx)
  (syntax-case stx ()
    [(_ mk args ...)
     (with-syntax ([(t ...) (generate-temporaries #'(args ...))])
       (syntax/loc stx
         (let ([t args] ...)
           (if (or (Bottom? t) ...)
               -Bottom
               (mk t ...)))))]))


(define/decl -Boolean (Un -False -True))

(define -val make-Value)
(define/decl -Null (-val null))

;; Char type and List type (needed because of how sequences are checked in subtype)
(define (make-Listof elem) (unsafe-make-Mu  (Un -Null (make-Pair elem (make-B 0)))))
(define (make-MListof elem) (unsafe-make-Mu (Un -Null (make-MPair elem (make-B 0)))))
(define (make-CyclicListof cycle)
  (cond
    [(ormap Bottom? cycle) -Null]
    [else (unsafe-make-Mu (Un -Null (-Tuple* cycle (make-B 0))))]))

;; -Tuple Type is needed by substitute for ListDots
(define -pair make-Pair)
(define (-lst* #:tail [tail -Null] . args)
  (for/fold ([tl tail]) ([a (in-list (reverse args))]) (-pair a tl)))
(define (-Tuple l)
  (-Tuple* l -Null))
(define (-Tuple* l b)
  (foldr -pair b l))

;; Recursive types
(define-syntax -v
  (syntax-rules ()
    [(_ x) (make-F 'x)]))

(define-syntax -mu
  (syntax-rules ()
    [(_ var ty)
     (let ([var (-v var)])
       (make-Mu 'var ty))]))

;; Results
(define/cond-contract (-result t [pset -tt-propset] [o -empty-obj])
  (c:->* (Type?) (PropSet? OptObject?) Result?)
  (cond
    [(or (equal? t -Bottom) (equal? pset -ff-propset))
     (make-Result -Bottom -ff-propset o)]
    [else
     (make-Result t pset o)]))

(define -values
  (case-lambda
    [(t ps o)
     (match t
       [(? Type?)
        (make-Values (list (-result t ps o)))]
       [(? list?) (make-Values (map -result t ps o))])]
    [(t) (match t
           [(? Type?)
            (make-Values (list (-result t)))]
           [(? Result?) (make-Values (list t))]
           [(? list? l)
            (cond
              [(or (null? l) (ormap Type? l))
               (make-Values (for/list ([type (in-list l)]) (-result type)))]
              [(ormap Result? l)
               (make-Values l)]
              [else (error '-values "invalid arg for -values: ~a" l)])])]))


;; Propositions
(define/decl -tt-propset (make-PropSet -tt -tt))
(define/decl -ff-propset (make-PropSet -ff -ff))

(define (-arg-path arg [depth 0])
  (make-Path null (cons depth arg)))
(define (-acc-path path-elems o)
  (match o
    [(Empty:) -empty-obj]
    [(Path: p o) (make-Path (append path-elems p) o)]))

(define/cond-contract (-PS + -)
  (c:-> Prop? Prop? PropSet?)
  (make-PropSet + -))


;; A Type that corresponds to the any contract for the
;; return type of functions
(define (-AnyValues f) (make-AnyValues f))
(define/decl ManyUniv (make-AnyValues -tt))

(define/decl -HashTableTop (Un (make-Immutable-HashTable Univ Univ)
                               -Mutable-HashTableTop
                               -Weak-HashTableTop))

(define/decl -VectorTop (Un (make-Immutable-Vector Univ)
                            -Mutable-VectorTop))

(define-match-expander VectorTop:
  (lambda (stx)
    (syntax-parse stx
     [(_)
      #'(or (Immutable-Vector: (Univ:))
            (? Mutable-VectorTop?))])))

;; Function types
(define/cond-contract (-Arrow dom rng
                              #:rest [rst #f]
                              #:kws [kws null]
                              #:props [props -tt-propset]
                              #:object [obj -empty-obj])
  (c:->* ((c:listof Type?) (c:or/c SomeValues? Type?))
         (#:rest (c:or/c #f Type? RestDots? Rest?)
          #:kws (c:listof Keyword?)
          #:props PropSet?
          #:object OptObject?)
         Arrow?)
  (make-Arrow dom
              (if (Type? rst) (make-Rest (list rst)) rst)
              (sort kws Keyword<?)
              (match rng
                [(? SomeValues?) rng]
                [_ (-values rng props obj)])))

(define-syntax (->* stx)
  (define-syntax-class c
    (pattern x:id #:fail-unless (eq? ': (syntax-e #'x)) #f))
  (syntax-parse stx
    [(_ dom rng)
     (syntax/loc stx
       (make-Fun (list (-Arrow dom rng))))]
    [(_ dom rst rng)
     (syntax/loc stx
       (make-Fun (list (-Arrow dom rng #:rest rst))))]
    [(_ dom rng :c props)
     (syntax/loc stx
       (make-Fun (list (-Arrow dom rng #:props props))))]
    [(_ dom rng _:c props _:c object)
     (syntax/loc stx
       (make-Fun (list (-Arrow dom rng #:props props #:object object))))]
    [(_ dom rst rng _:c props)
     (syntax/loc stx
       (make-Fun (list (-Arrow dom rng #:rest rst #:props props))))]
    [(_ dom rst rng _:c props : object)
     (syntax/loc stx
       (make-Fun (list (-Arrow dom rng #:rest rst #:props props #:object object))))]))

(define-syntax (-> stx)
  (define-syntax-class c
    (pattern x:id #:fail-unless (eq? ': (syntax-e #'x)) #f))
  (syntax-parse stx
    [(_ dom ... rng _:c props _:c objects)
     (syntax/loc stx
       (->* (list dom ...) rng : props : objects))]
    [(_ dom ... rng :c props)
     (syntax/loc stx
       (->* (list dom ...) rng : props))]
    [(_ dom ... rng)
     (syntax/loc stx
       (->* (list dom ...) rng))]))

(define-syntax (->... stx)
  (syntax-parse stx
    [(_ dom rng) (syntax/loc stx (->* dom rng))]
    [(_ dom (dty dbound) rng)
     (syntax/loc stx
       (make-Fun
        (list (-Arrow dom rng #:rest (make-RestDots dty 'dbound)))))]
    [(_ dom rng (~datum :) props)
     (syntax/loc stx
       (->* dom rng (~datum :) props))]
    [(_ dom (dty dbound) rng (~datum :) props)
     (syntax/loc stx
       (make-Fun
        (list (-Arrow dom rng
                      #:rest (make-RestDots dty 'dbound)
                      #:props props))))]))

(define (simple-> doms rng)
  (->* doms rng))

(define (->acc dom rng path #:var [var (cons 0 0)])
  (define obj (-acc-path path (-id-path var)))
  (make-Fun
   (list (-Arrow dom rng
                 #:props (-PS (-not-type obj (-val #f))
                              (-is-type obj (-val #f)))
                 #:object obj))))

(define (cl->* . args)
  (make-Fun (apply append (map Fun-arrows args))))

(define-syntax (cl-> stx)
  (syntax-parse stx
    [(_ [(dom ...) rng] ...)
     (syntax/loc stx
       (cl->* (dom ... . -> . rng) ...))]))

(define-syntax (->key stx)
  (syntax-parse stx
    [(_ ty:expr ... (~seq k:keyword kty:expr opt:boolean) ... rng)
     (syntax/loc stx
       (make-Fun
        (list
         (-Arrow (list ty ...)
                 rng
                 #:kws (sort (list (make-Keyword 'k kty opt) ...)
                             Keyword<?)))))]))

(define-syntax (->optkey stx)
  (syntax-parse stx
    [(_ ty:expr ... [oty:expr ...] #:rest rst:expr (~seq k:keyword kty:expr opt:boolean) ... rng)
     (let ([l (syntax->list #'(oty ...))])
       (with-syntax ([((extra ...) ...)
                      (for/list ([i (in-range (add1 (length l)))])
                        (take l i))]
                     ;; only the LAST arrow gets a #:rest arg
                     [(rst ...) (for/list ([i (in-range (add1 (length l)))])
                                  (if (< i (length l)) #'#f #'rst))])
         (syntax/loc stx
           (make-Fun
            (list
             (-Arrow (list ty ... extra ...)
                     rng
                     #:rest rst
                     #:kws (sort (list (make-Keyword 'k kty opt) ...)
                                 Keyword<?))
             ...)))))]
    [(_ ty:expr ... [oty:expr ...] (~seq k:keyword kty:expr opt:boolean) ... rng)
     (let ([l (syntax->list #'(oty ...))])
       (with-syntax ([((extra ...) ...)
                      (for/list ([i (in-range (add1 (length l)))])
                        (take l i))])
         (syntax/loc stx
           (make-Fun
            (list
             (-Arrow (list ty ... extra ...)
                        rng
                        #:kws (sort (list (make-Keyword 'k kty opt) ...)
                                    Keyword<?))
             ...)))))]))

(define (make-arr-dots dom rng dty dbound)
  (-Arrow dom rng #:rest (make-RestDots dty dbound)))


(define-syntax (dep-> stx)
  (syntax-parse stx
    [(_ ([x:id (~datum :) dom:expr] ...)
        (~optional (~seq #:pre pre)
                   #:defaults ([pre #'-tt]))
        rng:expr
        (~optional (~or (~seq (~datum :) props:expr)
                        (~seq (~datum :) props:expr
                              (~datum :) object:expr))
                   #:defaults ([props #'-tt-propset]
                               [object #'-empty-obj])))
     (with-syntax ([(d ...) (generate-temporaries #'(dom ...))]
                   [(d* ...) (generate-temporaries #'(dom ...))])
       (syntax/loc stx
         (let ([x (genid)] ...)
           (let ([ids (list x ...)]
                 [d dom] ...
                 [p pre]
                 [r rng]
                 [ps props]
                 [o object])
             (let ([d* (abstract-obj d ids)] ...
                   [p* (abstract-obj p ids)]
                   [r* (abstract-obj r ids)]
                   [ps* (abstract-obj ps ids)]
                   [o* (abstract-obj o ids)])
               (cond
                 [(and (equal? d d*) ... (equal? r r*) (TrueProp? p*))
                  ;; non-dependent (as in DFun) case!
                  (-Arrow (list d* ...)
                          (make-Values (list (-result r* ps* o*))))]
                 ;; one of the domains or the range was dependent!
                 [else
                  (define (arg? id)
                    (member id ids free-identifier=?))
                  (define dom-deps (for/list ([id (in-list ids)]
                                              [ty (in-list (list d ...))])
                                     (cons id (filter arg? (free-ids ty)))))
                  (define cycle (cycle-in-arg-deps? dom-deps))
                  (cond
                    [cycle
                     (error 'dep-> "cyclic dependency: ~a" cycle)]
                    [else
                     (make-DepFun
                      (list d* ...)
                      p*
                      (-values r* ps* o*))])]))))))]))


;; Convenient syntax for polymorphic types
(define-syntax -poly
  (syntax-rules ()
    [(_ (vars ...) ty)
     (let ([vars (-v vars)] ...)
       (make-Poly (list 'vars ...) ty))]))

(define-syntax -polydots
  (syntax-rules ()
    [(_ (vars ... dotted) ty)
     (let ([dotted (-v dotted)]
           [vars (-v vars)] ...)
       (make-PolyDots (list 'vars ... 'dotted) ty))]))

(define-syntax -polyrow
  (syntax-rules ()
    [(_ (var) consts ty)
     (let ([var (-v var)])
       (make-PolyRow (list 'var) consts ty))]))
