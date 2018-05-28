#lang racket/base

(require "../utils/utils.rkt"
         (prefix-in c: (contract-req))
         (types abbrev subtype numeric-tower prop-ops)
         (types tc-result type-table)
         racket/match
         syntax/private/id-table
         syntax/parse
         (for-syntax racket/base))

(provide/cond-contract
 [has-linear-integer-refinements? (c:-> identifier? boolean?)]
 [maybe-add-linear-integer-refinements (c:-> identifier? syntax? full-tc-results/c
                                             full-tc-results/c)])

(define (has-linear-integer-refinements? id)
  (and (free-id-table-ref linear-integer-function-table id #f) #t))

(define (maybe-add-linear-integer-refinements id args-stx result)
  (cond
    [(free-id-table-ref linear-integer-function-table id #f)
     => (λ (f) (f args-stx result))]
    [else result]))

;; takes a result and adds p to the then proposition
;; and (not p) to the else proposition
(define (add-p/not-p result p)
  (match result
    [(tc-result1: t (PropSet: p+ p-) o)
     (ret t
          (-PS (-and p p+) (-and (negate-prop p) p-))
          o)]
    [_ result]))

(define (add-to-pos-side result p)
  (match result
    [(tc-result1: t (PropSet: p+ p-) o)
     (ret t
          (-PS (-and p p+) p-)
          o)]
    [_ result]))

;; class to recognize expressions that typecheck at a subtype of `type`
(define-syntax-class (w/obj+type type)
  #:attributes (obj)
  (pattern e:expr
           #:do [(define o
                   (match (type-of #'e)
                     [(tc-result1: t _ (? Object? o))
                      #:when (subtype t type)
                      o]
                     [_ #f]))]
           #:fail-unless o (format "not a ~a expr with a non-empty object" type)
           #:attr obj o))

(define-syntax-class (w/type type)
  #:attributes (obj)
  (pattern e:expr
           #:do [(define o
                   (match (type-of #'e)
                     [(tc-result1: t _ o)
                      #:when (subtype t type)
                      o]
                     [_ #f]))]
           #:fail-unless o (format "not a ~a expr with a non-empty object" type)
           #:attr obj (if (Object? o) o -empty-obj)))

;; < <= >= =
(define (numeric-comparison-function prop-constructor)
  (λ (args-stx result)
    (syntax-parse args-stx
      [((~var e1 (w/obj+type -Int)) (~var e2 (w/obj+type -Int)))
       (define p (prop-constructor (attribute e1.obj) (attribute e2.obj)))
       (add-p/not-p result p)]
      [((~var e1 (w/type -Int)) (~var e2 (w/type -Int)) (~var e3 (w/type -Int)))
       #:when (or (and (Object? (attribute e1.obj)) (Object? (attribute e2.obj)))
                  (and (Object? (attribute e2.obj)) (Object? (attribute e3.obj))))
       (define p (-and (prop-constructor (attribute e1.obj) (attribute e2.obj))
                       (prop-constructor (attribute e2.obj) (attribute e3.obj))))
       (add-p/not-p result p)]
      [_  result])))

;; +/-
(define (plus/minus plus?)
  (λ (args-stx result)
    (match result
      [(tc-result1: ret-t ps orig-obj)
       (syntax-parse args-stx
         ;; +/- (2 args)
         [((~var e1 (w/obj+type -Int))
           (~var e2 (w/obj+type -Int)))
          (define (sign o) (if plus? o (scale-obj -1 o)))
          (define l (-lexp (attribute e1.obj) (sign (attribute e2.obj))))
          (ret (-refine/fresh x ret-t (-eq (-lexp x) l))
               ps
               l)]
         ;; +/- (3 args)
         [((~var e1 (w/obj+type -Int))
           (~var e2 (w/obj+type -Int))
           (~var e3 (w/obj+type -Int)))
          (define (sign o) (if plus? o (scale-obj -1 o)))
          (define l (-lexp (attribute e1.obj) (sign (attribute e2.obj)) (sign (attribute e3.obj))))
          (ret (-refine/fresh x ret-t (-eq (-lexp x) l))
               ps
               l)]
         [_ result])]
      [_ result])))

;; equal?/eqv?/eq?
;; if only one side is a supported type, we can learn integer equality for
;; a result of `#t`, whereas if both sides are of the supported type,
;; we learn on both `#t` and `#f` answers
(define (equality-function supported-type)
  (λ (args-stx result)
    (syntax-parse args-stx
      [((~var e1 (w/obj+type supported-type)) (~var e2 (w/obj+type supported-type)))
       (define p (-eq (attribute e1.obj) (attribute e2.obj)))
       (add-p/not-p result p)]
      [((~var e1 (w/obj+type supported-type)) (~var e2 (w/obj+type Univ)))
       (define p (-eq (attribute e1.obj) (attribute e2.obj)))
       (add-to-pos-side result p)]
      [((~var e1 (w/obj+type Univ)) (~var e2 (w/obj+type supported-type)))
       (define p (-eq (attribute e1.obj) (attribute e2.obj)))
       (add-to-pos-side result p)]
      [_  result])))

;;  *
(define product-function
  (λ (args-stx result)
    (match result
      [(tc-result1: ret-t ps orig-obj)
       (syntax-parse args-stx
         [((~var e1 (w/obj+type -Int)) (~var e2 (w/obj+type -Int)))
          (define product-obj (-obj* (attribute e1.obj) (attribute e2.obj)))
          (cond
            [(Object? product-obj)
             (ret (-refine/fresh x ret-t (-eq (-lexp x) product-obj))
                  ps
                  product-obj)]
            [else result])]
         [_ result])]
      [_ result])))

;; make-vector
(define make-vector-function
  (λ (args-stx result)
    (match result
      [(tc-result1: ret-t ps orig-obj)
       (syntax-parse args-stx
         [((~var size (w/obj+type -Int)) . _)
          (ret (-refine/fresh v ret-t (-eq (-lexp (-vec-len-of (-id-path v)))
                                           (attribute size.obj)))
               ps
               orig-obj)]
         [_ result])]
      [_ result])))

;; modulo
(define modulo-function
  (λ (args-stx result)
    (match result
      [(tc-result1: ret-t ps orig-obj)
       (syntax-parse args-stx
         [((~var e1 (w/type -Int)) (~var e2 (w/obj+type -Nat)))
          (ret (-refine/fresh x ret-t (-lt (-lexp x) (attribute e2.obj)))
               ps
               orig-obj)]
         [_ result])]
      [_ result])))

;; random
(define random-function
  (λ (args-stx result)
    (match result
      [(tc-result1: ret-t ps orig-obj)
       (syntax-parse args-stx
         ;; random (1 arg)
         [((~var e1 (w/obj+type -Nat)))
          (ret (-refine/fresh x ret-t (-lt (-lexp x) (attribute e1.obj)))
               ps
               orig-obj)]
         ;; random (2 arg)
         [((~var e1 (w/type -Int)) (~var e2 (w/type -Int)))
          #:when (or (Object? (attribute e1.obj))
                     (Object? (attribute e2.obj)))
          (ret (-refine/fresh x ret-t (-and (-leq (attribute e1.obj) (-lexp x))
                                            (-lt (-lexp x) (attribute e2.obj))))
               ps
               orig-obj)]
         [_ result])]
      [_ result])))

;; add1 / sub1
(define (add/sub-1-function add?)
  (λ (args-stx result)
    (match result
      [(tc-result1: ret-t ps orig-obj)
       (syntax-parse args-stx
         [((~var e1 (w/obj+type -Int)))
          (define l ((if add? -lexp-add1 -lexp-sub1) (attribute e1.obj)))
          (ret (-refine/fresh x ret-t (-eq (-lexp x) l))
               ps
               l)]
         [_ result])]
      [_ result])))

(define linear-integer-function-table
  (make-immutable-free-id-table
   (list
    (cons #'< (numeric-comparison-function -lt))
    (cons #'<= (numeric-comparison-function -leq))
    (cons #'> (numeric-comparison-function -gt))
    (cons #'>= (numeric-comparison-function -geq))
    (cons #'= (numeric-comparison-function -eq))
    (cons #'eqv? (equality-function -Int))
    (cons #'equal? (equality-function -Int))
    (cons #'eq? (equality-function -Fixnum))
    (cons #'+ (plus/minus #t))
    (cons #'- (plus/minus #f))
    (cons #'* product-function)
    (cons #'make-vector make-vector-function)
    (cons #'modulo modulo-function)
    (cons #'random random-function)
    (cons #'add1 (add/sub-1-function #t))
    (cons #'sub1 (add/sub-1-function #f)))
   #:phase -1))

