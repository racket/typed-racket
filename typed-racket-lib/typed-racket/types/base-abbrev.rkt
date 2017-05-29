#lang racket/base

;; This file is for the abbreviations needed to implement union.rkt
;;
;; The "abbrev.rkt" module imports this module, re-exports it, and
;; extends it with more types and type abbreviations.

(require "../utils/utils.rkt"
         "../rep/type-rep.rkt"
         "../rep/prop-rep.rkt"
         "../rep/object-rep.rkt"
         "../rep/base-types.rkt"
         "../rep/numeric-base-types.rkt"
         (utils tc-utils)
         (rep values-rep rep-utils)
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
(define (make-Listof elem) (-mu list-rec (Un -Null (make-Pair elem list-rec))))
(define (make-MListof elem) (-mu list-rec (Un -Null (make-MPair elem list-rec))))

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

;; Propositions
(define/decl -tt-propset (make-PropSet -tt -tt))
(define/decl -ff-propset (make-PropSet -ff -ff))

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

;; Function types
(define/cond-contract (-Arr dom rng
                            #:rest [rst #f] #:drest [drst #f]
                            #:kws [kws null]
                            #:props [props #f] #:object [obj #f])
  (c:->* ((c:listof Type?) (c:or/c SomeValues? Type?))
         (#:rest (c:or/c #f Type?)
          #:drest (c:or/c #f Type? RestDots?)
          #:kws (c:listof Keyword?)
          #:props PropSet?
          #:object OptObject?)
         arr?)
  (let ([rng (if (Type? rng)
                 (make-Values (list (-result rng props obj)))
                 rng)])
    (cond
      ;; simple arrows (Arrow)
      [(not (or rst drst (pair? kws) props obj))
       (make-ArrowSimp dom rng)]
      ;; complex arrows (ArrowStar)
      [(and (or rst drst (pair? kws))
            (not (or props obj)))
       (when (and rst drst)
         (int-err "-Arr: rst and drst provided: ~a ~a" rst drst))
       (let ([rst (or rst drst)])
         (make-ArrowStar dom rst kws rng))]
      ;; dependent arrows (ArrowDep)
      [(or props obj)
       (when drst
         (int-err "-Arr: drst provided for dependent arrow: ~a" drst))
       (unless (null? kws)
         (int-err "-Arr: dependent arrow cannot have kws, given ~a" kws))
       (let ([rst (or rst drst)])
         (make-ArrowDep dom rst rng))]
      [else
       (int-err
        "unsupported -Arr args: ~a ~a ~a ~a ~a ~a ~a"
        (format "dom ~a" rst)
        (format "rng ~a" rst)
        (format "#:rest ~a" rst)
        (format "#:drest ~a" drst)
        (format "#:kws ~a" kws)
        (format "#:props ~a" props)
        (format "#:object ~a" obj))])))

(begin-for-syntax
  (define-syntax-class c
    (pattern x:id #:fail-unless (eq? ': (syntax-e #'x)) #f)))

(define-syntax (->* stx)
  (syntax-parse stx
    [(_ dom rng)
     (syntax/loc stx
       (make-Function (list (-Arr dom rng))))]
    [(_ dom rst rng)
     (syntax/loc stx
       (make-Function (list (make-arr* dom rng #:rest rst))))]))

(define-syntax (dep-> stx)
  (syntax-parse stx
    [(_ dom rng _:c props)
     (syntax/loc stx (dep-> dom #f rng : props : -empty-obj))]
    [(_ dom rng _:c props _:c object)
     (syntax/loc stx (dep-> dom #f rng : props : object))]
    [(_ dom rst rng _:c props)
     (syntax/loc stx (dep-> dom rst rng : props : object))]
    [(_ ([x0:id _:c t0:expr] [x-rst:id _:c t-rst:expr] ...)
        rst
        rng
        _:c props
        _:c object)
     (define xs (syntax->list #'(x0 x-rst ...)))
     (with-syntax
         ([(dom-id ...) (generate-temporaries #'(x0 x-rst ...))])
       (with-syntax
           ([(domain-bindings ...)
             (apply
              append
              (for/list ([x (in-list xs)]
                         [dom (in-list (syntax->list #'(dom-id ...)))]
                         [dom-ty (in-list (syntax->list #'(t0 t-rst ...)))]
                         [n (in-naturals)])
                ;; each domain arg is in scope for
                ;; domain types that proceed it
                (with-syntax ([(n-xs ...) (take xs n)])
                  (list #`[#,dom (abstract-many/obj #,dom-ty (list n-xs ...))]
                        #`[#,x (quote-syntax #,x)]))))])
         (syntax/loc stx
           (let* (domain-bindings ...
                  [names (list (quote-syntax x0) (quote-syntax x-rst) ...)]
                  [domain (list dom-id ...)])
             (make-Function
              (list
               (-Arr domain
                     (abstract-many/obj rng names)
                     #:rest (and rst (abstract-many/obj rst names))
                     #:props (abstract-many/obj props names)
                     #:object (abstract-many/obj object names))))))))]))


(define-syntax (-> stx)
  (syntax-parse stx
    [(_ dom rng _:c props _:c objects)
     (syntax/loc stx (dep-> dom rng : props : objects))]
    [(_ dom rng :c props)
     (syntax/loc stx (dep-> dom  rng : props))]
    [(_ dom ... rng)
     (syntax/loc stx (->* (list dom ...) rng))]))

(define-syntax (->... stx)
  (syntax-parse stx
    [(_ dom rng) (syntax/loc stx (->* dom rng))]
    [(_ dom (dty dbound) rng)
     (syntax/loc stx
       (make-Function
        (list (-Arr dom rng #:drest (make-RestDots dty 'dbound)))))]))

(define (simple-> doms rng)
  (-Arr doms rng))

;; specify a function which is an accessor into one
;; of its arguments (but use a macro so use sites
;; don't have to worry about our binder representation)
(define-syntax (->acc stx)
  (syntax-parse stx
    [(_ ([x:id _:c ty:expr] ...)
        rng:expr
        path:expr
        #:arg arg:id)
     (define n (length (syntax->list #'(x ...))))
     (with-syntax ([(idx ...) (range (sub1 n) -1 -1)])
       (syntax/loc stx
         (let ([obj (-acc-path path
                               (-id-path (let ([x idx] ...)
                                           arg)))])
           (make-Function
            (list (-Arr (list ty ...)
                        rng
                        #:props (-PS (-not-type obj (-val #f))
                                     (-is-type obj (-val #f)))
                        #:object obj))))))]))

(define (cl->* . args)
  (make-Function (apply append (map Function-arrows args))))

(define-syntax (cl-> stx)
  (syntax-parse stx
    [(_ [(dom ...) rng] ...)
     (syntax/loc stx
       (cl->* (dom ... . -> . rng) ...))]))

(define-syntax (->key stx)
  (syntax-parse stx
    [(_ ty:expr ... (~seq k:keyword kty:expr opt:boolean) ... rng)
     (syntax/loc stx
       (make-Function
        (list
         (make-arr* (list ty ...)
                    rng
                    #:kws (sort #:key Keyword-kw
                                (list (make-Keyword 'k kty opt) ...)
                                keyword<?)))))]))

(define-syntax (->optkey stx)
  (syntax-parse stx
    [(_ ty:expr ... [oty:expr ...]
        #:rest rst:expr
        (~seq k:keyword kty:expr opt:boolean) ...
        rng)
     (let ([l (syntax->list #'(oty ...))])
       (with-syntax ([((extra ...) ...)
                      (for/list ([i (in-range (add1 (length l)))])
                        (take l i))]
                     [(rsts ...) (for/list ([i (in-range (add1 (length l)))]) #'rst)])
         (syntax/loc stx
           (make-Function
            (list
             (-Arr (list ty ... extra ...)
                   rng
                   #:rest rsts
                   #:kws (sort #:key Keyword-kw
                               (list (make-Keyword 'k kty opt) ...)
                               keyword<?))
             ...)))))]
    [(_ ty:expr ...
        [oty:expr ...]
        (~seq k:keyword kty:expr opt:boolean) ...
        rng)
     (let ([l (syntax->list #'(oty ...))])
       (with-syntax ([((extra ...) ...)
                      (for/list ([i (in-range (add1 (length l)))])
                        (take l i))])
         (syntax/loc stx
           (make-Function
            (list
             (-Arr (list ty ... extra ...)
                   rng
                   #:rest #f
                   #:kws (sort #:key Keyword-kw
                               (list (make-Keyword 'k kty opt) ...)
                               keyword<?))
             ...)))))]))

(define (make-arr-dots dom rng dty dbound)
  (-Arr dom rng #:drest (make-RestDots dty dbound)))

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

