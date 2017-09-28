#lang racket/unit


(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse syntax/stx racket/match racket/sequence
         (typecheck signatures tc-funapp error-message)
         (types abbrev type-table utils substitute)
         (rep type-rep)
         (env tvar-env)
         (prefix-in i: (infer infer))

         (for-label
          racket/base
          (only-in '#%kernel [reverse k:reverse])))


(import tc-expr^ tc-app^)
(export tc-app-list^)

(define-literal-set list-literals
  #:for-label
  (reverse k:reverse list list* cons map andmap ormap))

(define-syntax-class boolmap
  #:literal-sets (list-literals)
  #:attributes (default)
  (pattern andmap #:attr default #t)
  (pattern ormap #:attr default #f))

(define-tc/app-syntax-class (tc/app-list expected)
  #:literal-sets (list-literals)
  (pattern (~and form (map f arg0 arg ...))
    (match* ((single-value #'arg0) (stx-map single-value #'(arg ...)))
      ;; if the argument is a ListDots
      [((tc-result1: (ListDots: t0 bound0))
        (list (tc-result1: (or (and (ListDots: t bound))
                               ;; NOTE: safe to include these, `map' will error if any list is
                               ;; not the same length as all the others
                               (and (Listof: t) (bind bound #f ))))
              ...))
       (=> fail)
       (unless (for/and ([b (in-list bound)]) (or (not b) (eq? bound0 b))) (fail))
       (define expected-elem-type
         (match expected
           [(or #f (tc-any-results: _)) #f]
           [(tc-result1: (ListDots: elem-type (== bound0))) (ret elem-type)]
           [(tc-result1: (Listof: elem-type)) (ret elem-type)]
           [else (fail)]))
       ;; Do not check this in an environment where bound0 is a type variable.
       (define f-type (tc-expr/t #'f))
       ;; Check that the function applies successfully to the element type
       ;; We need the bound to be considered a type var here so that inference works
       (match (extend-tvars (list bound0)
                (tc/funapp #'f #'(arg0 arg ...) f-type (cons (ret t0) (map ret t))
                           expected-elem-type))
         [(tc-result1: t) (ret (make-ListDots t bound0))]
         [(tc-results: (list (tc-result: ts)) _)
          (tc-error/expr "Expected one value, but got ~a" (-values ts))])]
      ;; otherwise, if it's not a ListDots, defer to the regular function typechecking
      ;; TODO fix double typechecking
      [(res0 res) (tc/app-regular #'form expected)]))
  ;; ormap/andmap of ... argument
  (pattern (~and form (m:boolmap f arg))
    (match-let* ([arg-ty (tc-expr/t #'arg)]
                 [ft (tc-expr/t #'f)])
      (match (match arg-ty
               ;; if the argument is a ListDots
               [(ListDots: t bound)
                ;; just check that the function applies successfully to the element type
                (extend-tvars (list bound)
                  (tc/funapp #'f #'(arg) ft (list (ret t)) expected))]
               ;; otherwise ...
               [_ #f])
        [(tc-result1: t) (ret (Un (-val (attribute m.default)) t))]
        ;; if it's not a ListDots, defer to the regular function typechecking
        ;; TODO fix double typechecking
        [_ (tc/app-regular #'form expected)])))
  ;; special case for `list'
  (pattern
   ((~and op-name list) . args)
   (let ([args-list (syntax->list #'args)])
     (match expected
       [(tc-result1: t)
        (match t
          [(List: ts)
           (cond
             [(= (length ts) (length args-list))
              (for ([arg (in-list args-list)]
                    [t (in-list ts)])
                (tc-expr/check arg (ret t)))
              (add-typeof-expr #'op-name (ret (->* ts t)))
              (ret t)]
             [else
              (expected-but-got t (-Tuple (map tc-expr/t args-list)))
              (ret t)])]
          [_
           (define vs (map (λ (_) (gensym)) args-list))
           (define l-type (-Tuple (map make-F vs)))
           ;; We want to infer the largest vs that are still under the element types
           (define substs (i:infer vs null (list l-type) (list t) (-values (list (-> l-type Univ)))
                                   #:multiple? #t))
           (cond
             [substs
              (define result
                (for*/first ([subst (in-list substs)]
                             [arg-tys (in-value (for/list ([arg (in-list args-list)]
                                                           [v (in-list vs)])
                                                  (tc-expr/check/t? arg (ret (subst-all subst (make-F v))))))]
                             #:when (andmap values arg-tys))
                  (define return-ty (-Tuple arg-tys))
                  (add-typeof-expr #'op-name (ret (->* arg-tys return-ty)))
                  (ret return-ty)))
              (or result
                  (begin (expected-but-got t (-Tuple (map tc-expr/t args-list)))
                         (fix-results expected)))]
             [else
              (define arg-tys (map tc-expr/t args-list))
              (define return-ty (-Tuple arg-tys))
              (add-typeof-expr #'op-name (ret (->* arg-tys return-ty)))
              (ret return-ty)])])]
       [_
        (define arg-tys (map tc-expr/t args-list))
        (define return-ty (-Tuple arg-tys))
        (add-typeof-expr #'op-name (ret (->* arg-tys return-ty)))
        (ret return-ty)])))
  ;; special case for `list*'
  (pattern ((~and op-name list*) (~between args:expr 1 +inf.0) ...)
    (match-let* ([(and arg-tys (list tys ... last)) (stx-map tc-expr/t #'(args ...))])
      (define return-ty (foldr -pair last tys))
      (add-typeof-expr #'op-name (ret (->* arg-tys return-ty)))
      (ret return-ty)))
  ;; special case for `reverse' to propagate expected type info
  (pattern ((~and fun (~or reverse k:reverse)) arg)
    (match expected
      [(tc-result1: (and return-ty (Listof: _)))
       (begin0
         (tc-expr/check #'arg expected)
         (add-typeof-expr #'fun (ret (-> return-ty return-ty))))]
      [(tc-result1: (List: ts))
       (define arg-ty (-Tuple (reverse ts)))
       (define return-ty (-Tuple ts))
       (tc-expr/check #'arg (ret arg-ty))
       (add-typeof-expr #'fun (ret (-> arg-ty return-ty)))
       (ret return-ty)]
      [_
       (match (single-value #'arg)
         [(tc-result1: (and arg-ty (List: ts)))
          (define return-ty (-Tuple (reverse ts)))
          (add-typeof-expr #'fun (ret (-> arg-ty return-ty)))
          (ret return-ty)]
         [(tc-result1: (and r (Listof: t)))
          (add-typeof-expr #'fun (ret (-> r r)))
          (ret r)]
         [arg-ty
          (tc/funapp #'fun #'(arg) (tc-expr/t #'fun) (list arg-ty) expected)])])))
