#lang racket/unit

(require "../../utils/utils.rkt"
         racket/list
         syntax/parse syntax/stx racket/match racket/sequence
         (for-syntax racket/base syntax/parse racket/syntax)
         "signatures.rkt"
         "utils.rkt"
         (utils prefab)
         (only-in (infer infer) intersect)
         (types utils abbrev numeric-tower resolve type-table
                generalize match-expanders)
         (typecheck signatures check-below)
         (rep type-rep type-mask rep-utils)
         (for-label racket/unsafe/ops racket/base))

(import tc-expr^ tc-app^ tc-literal^)
(export tc-app-hetero^)

(define-literal-set hetero-literals
  #:for-label
  (vector-ref unsafe-vector-ref unsafe-vector*-ref
   vector-set! unsafe-vector-set! unsafe-vector*-set!
   unsafe-struct-ref unsafe-struct*-ref
   unsafe-struct-set! unsafe-struct*-set!
   vector-immutable vector))

(define (tc/index expr)
  (syntax-parse expr
   #:literal-sets (kernel-literals)
   [(quote i:number)
    (let ((type (tc-literal #'i)))
      (add-typeof-expr expr (ret type))
      (syntax-e #'i))]
   [_
    (match (tc-expr expr)
     [(tc-result1: (Val-able: (? number? i))) i]
     [tc-results
       (check-below tc-results (ret -Integer))
       #f])]))

(define (index-error i-val i-bound expr type name)
  (cond
    [(not (and (integer? i-val) (exact? i-val)))
     (tc-error/expr #:stx expr "expected exact integer for ~a index, but got ~a" name i-val)]
    [(< i-val 0)
     (tc-error/expr #:stx expr "index ~a too small for ~a ~a" i-val name type)]
    [(not (< i-val i-bound))
     (tc-error/expr #:stx expr "index ~a too large for ~a ~a" i-val name type)]))

(define (valid-index? i bound)
 (and (integer? i) (exact? i) (<= 0 i (sub1 bound))))


;; FIXME - Do something with paths in the case that a structure/vector is not mutable
(define (tc/hetero-ref i-e es-t vec-t name op)
  (define i-val (tc/index i-e))
  (define i-bound (length es-t))
  (cond
    [(valid-index? i-val i-bound)
     (define return-ty (list-ref es-t i-val))
     (add-typeof-expr op (ret (-> vec-t -Fixnum return-ty)))
     (ret return-ty)]
    [(not i-val)
     (define return-ty (apply Un es-t))
     (add-typeof-expr op (ret (-> vec-t -Fixnum return-ty)))
     (ret return-ty)]
    [else
     (index-error i-val i-bound i-e vec-t name)]))

(define (tc/hetero-set! i-e es-t val-e vec-t name op)
  (define i-val (tc/index i-e))
  (define i-bound (length es-t))
  (cond
    [(valid-index? i-val i-bound)
     (define val-t (list-ref es-t i-val))
     (tc-expr/check val-e (ret val-t))
     (add-typeof-expr op (ret (-> vec-t -Fixnum val-t -Void)))
     (ret -Void)]
    [(not i-val)
     (define val-res (single-value val-e))
     (for ((es-type (in-list es-t)))
       (check-below val-res (ret es-type)))
     (define val-t
       (match val-res [(tc-result1: t) t]))
     (add-typeof-expr op (ret (-> vec-t -Fixnum val-t -Void)))
     (ret -Void)]
    [else
     (single-value val-e)
     (index-error i-val i-bound i-e vec-t name)]))

;; hetero-vecs->elems : (Listof Type?) -> (U #f (Listof Type?))
;; If `vts` is a list of HeterogeneousVector types
;;  all with the same elements,
;;  return the elements.
;; Else return `#false`
(define (hetero-vecs->elems ts)
  (for/fold ([acc #t])
            ([t (in-list ts)])
    (and acc
         (match t
           [(HeterogeneousVector: es)
            (if (eq? acc #true)
              es
              (and (equal? acc es) acc))]
           [_ #f]))))

(define-tc/app-syntax-class (tc/app-hetero expected)
  #:literal-sets (hetero-literals)
  (pattern (~and form ((~and op (~or unsafe-struct-ref unsafe-struct*-ref)) struct:expr index:expr))
    (match (single-value #'struct)
      [(tc-result1: (app resolve t))
       (let loop ([struct-t t])
         (match struct-t
           [(Intersection: ts _)
            (for/first ([t (in-list ts)]
                        #:when (or (Prefab? t) (Struct? t) (PrefabTop? t)))
              (loop t))]
           [(Struct: _ _ (list (fld: flds _ _) ...) _ _ _ _)
            (tc/hetero-ref #'index flds struct-t "struct" #'op)]
           [(Prefab: _ (list flds ...))
            (tc/hetero-ref #'index flds struct-t "prefab struct" #'op)]
           [(PrefabTop: key)
            (tc/hetero-ref #'index
                           (build-list (prefab-key->field-count key) (λ (_) Univ))
                           struct-t
                           "prefab struct"
                           #'op)]
           [_ (tc/app-regular #'form expected)]))]))
  ;; vector-ref on het vectors
  (pattern (~and form ((~and op (~or vector-ref unsafe-vector-ref unsafe-vector*-ref)) vec:expr index:expr))
    (match (single-value #'vec)
      [(tc-result1: (and vec-t (app resolve (Is-a: (HeterogeneousVector: es)))))
       (tc/hetero-ref #'index es vec-t "vector" #'op)]
      [(tc-result1: (and vec-t (app resolve (Union: _ (app hetero-vecs->elems (? pair? es))))))
       (tc/hetero-ref #'index es vec-t "vector" #'op)]
      [v-ty (tc/app-regular #'form expected)]))
  ;; unsafe struct-set!
  (pattern (~and form ((~and op (~or unsafe-struct-set! unsafe-struct*-set!)) s:expr index:expr val:expr))
    (match (single-value #'s)
      [(tc-result1: (and struct-t (app resolve (Struct: _ _ (list (fld: flds _ _) ...) _ _ _ _))))
       (tc/hetero-set! #'index flds #'val struct-t "struct" #'op)]
      [s-ty (tc/app-regular #'form expected)]))
  ;; vector-set! on het vectors
  (pattern (~and form ((~and op (~or vector-set! unsafe-vector-set! unsafe-vector*-set!)) v:expr index:expr val:expr))
    (match (single-value #'v)
      [(tc-result1: (and vec-t (app resolve (Is-a: (HeterogeneousVector: es)))))
       (tc/hetero-set! #'index es #'val vec-t "vector" #'op)]
      [(tc-result1: (and vec-t (app resolve (Union: _ (app hetero-vecs->elems (? pair? es))))))
       (tc/hetero-set! #'index es #'val vec-t "vector" #'op)]
      [v-ty (tc/app-regular #'form expected)]))
  (pattern (~and form (vector-immutable args:expr ...))
    (tc-app-immutable-hetero-vector #'form expected))
  (pattern (~and form (vector args:expr ...))
    (tc-app-mutable-hetero-vector #'form expected)))

;; The cases for `(vector-immutable e ...)` and `(vector e ...)` are similar
;;  but use different match expanders, so we use a macro to abstract over the similar parts
;; The `#:with` clause below contains the differences
(define-values [tc-app-immutable-hetero-vector tc-app-mutable-hetero-vector]
  (let ()
    (define-syntax (make-tc-app-hetero-vector stx)
      (syntax-parse stx #:literals (quote)
       [(_ (quote mode:id))
        #:with (?make-HV ?match-V ?match-HV ?maskV ?tc-expr/t/maybe-generalize ?fn)
               (case (syntax-e #'mode)
                [(immutable)
                 (syntax/loc stx
                   (make-Immutable-HeterogeneousVector
                    Immutable-Vector:
                    Immutable-HeterogeneousVector:
                    mask:immutable-vector
                    tc-expr/t
                    "vector-immutable"))]
                [(mutable)
                 (syntax/loc stx
                   (make-Mutable-HeterogeneousVector
                    Mutable-Vector:
                    Mutable-HeterogeneousVector:
                    mask:mutable-vector
                    (lambda (e) (generalize (tc-expr/t e)))
                    "vector"))]
                [else
                 (raise-argument-error 'make-tc-app-hetero-vector "(or/c 'immutable 'mutable)" (syntax-e #'mode))])
        (syntax/loc stx
          (let ([has-vector-mask? (λ (t) (eq? ?maskV (mask t)))])
            (λ (form expected)
              (syntax-parse form #:literal-sets (hetero-literals)
               [((~and op (~or vector-immutable vector)) . args)
                (match expected
                 [(tc-result1: (app resolve (Is-a: (?match-V t))))
                  (define arg-tys
                    (for/list ([e (in-syntax #'args)])
                      (tc-expr/check e (ret t))
                      t))
                  (define return-ty
                    (?make-HV arg-tys))
                  (add-typeof-expr #'op (ret (->* arg-tys return-ty)))
                  (ret return-ty)]
                 [(tc-result1: (app resolve (Is-a: (?match-HV ts))))
                  (cond
                   [(= (length ts) (syntax-length #'args))
                    (define arg-tys
                      (for/list ([e (in-syntax #'args)]
                                 [t (in-list ts)])
                        (tc-expr/check/t e (ret t))))
                    (define return-ty
                      (?make-HV arg-tys))
                    (add-typeof-expr #'op (ret (->* arg-tys return-ty)))
                    (ret return-ty -true-propset)]
                   [else
                    (tc-error/expr
                      "expected vector with ~a elements, but got ~a"
                      (length ts) (?make-HV (stx-map tc-expr/t #'args)))])]
                 ;; If the expected type is a union, then we examine just the parts
                 ;; of the union that are vectors.  If there's only one of those,
                 ;; we re-run this whole algorithm with that.  Otherwise, we treat
                 ;; it like any other expected type.
                 [(tc-result1: (app resolve (Is-a: (Union: _ ts))))
                  #:when (= 1 (count has-vector-mask? ts))
                  (define t0 (findf has-vector-mask? ts))
                  (tc/app #`(#%plain-app . #,form) (ret t0))]
                 ;; if mutable, generalize element types
                 [_
                  (define tc-hv-elem ?tc-expr/t/maybe-generalize)
                  (define arg-tys
                    (for/list ((e (in-syntax #'args)))
                      (tc-hv-elem e)))
                  (define return-ty
                    (?make-HV arg-tys))
                  (add-typeof-expr #'op (ret (->* arg-tys return-ty)))
                  (ret return-ty)])]))))]))
  (values (make-tc-app-hetero-vector 'immutable) (make-tc-app-hetero-vector 'mutable))))
