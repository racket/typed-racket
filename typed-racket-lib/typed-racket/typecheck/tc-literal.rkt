#lang racket/unit

(require "../utils/utils.rkt"
         racket/match
         (typecheck signatures check-below)
         (types abbrev numeric-tower resolve subtype generalize
                prefab)
         (rep type-rep)
         (only-in (infer infer) intersect)
         (utils stxclass-util)
         syntax/parse
         racket/extflonum)

(import)
(export tc-literal^)

;; racket/function's conjoin is very general and therefore slow
;; and it's trivial to just define the specific version we need:
(define-syntax-rule (conjoin f ...)
  (λ (x) (and (f x) ...)))

(define (maybe-add-linear-integer-info int t)
  (if (with-refinements?)
      (-refine/fresh x t (-eq (-lexp x) (-lexp int)))
      t))

;; return the type of a literal value
;; tc-literal: racket-value-syntax [type] -> type
(define (tc-literal v-stx [expected #f])
  (define-syntax-class exp
    (pattern (~and i (~or :number :str :bytes :char))
             #:fail-unless expected #f
             #:fail-unless (let ([n (syntax-e #'i)])
                             (subtype (-val n) expected (if (exact-integer? n) (-lexp n) -empty-obj))) #f))
  (syntax-parse v-stx
    [i:exp expected]
    [i:boolean (-val (syntax-e #'i))]
    [i:identifier (-val (syntax-e #'i))]
    ;; Numbers
    [0 (maybe-add-linear-integer-info 0 -Zero)]
    [1 (maybe-add-linear-integer-info 1 -One)]
    [(~var i (3d (conjoin byte? positive?)))
     (maybe-add-linear-integer-info (syntax-e #'i) -PosByte)]
    [(~var i (3d byte?))
     (maybe-add-linear-integer-info (syntax-e #'i) -Byte)]
    [(~var i (3d (conjoin portable-index? positive?)))
     (maybe-add-linear-integer-info (syntax-e #'i) -PosIndex)]
    [(~var i (3d (conjoin portable-fixnum? positive?)))
     (maybe-add-linear-integer-info (syntax-e #'i) -PosFixnum)]
    [(~var i (3d (conjoin portable-fixnum? negative?)))
     (maybe-add-linear-integer-info (syntax-e #'i) -NegFixnum)]
    [(~var i (3d exact-positive-integer?))
     (maybe-add-linear-integer-info (syntax-e #'i) -PosInt)]
    [(~var i (3d (conjoin exact-integer? negative?)))
     (maybe-add-linear-integer-info (syntax-e #'i) -NegInt)]
    [(~var i (3d (conjoin number? exact? rational? positive?))) -PosRat]
    [(~var i (3d (conjoin number? exact? rational? negative?))) -NegRat]
    [(~var i (3d (lambda (x) (eqv? x 0.0)))) -FlonumPosZero]
    [(~var i (3d (lambda (x) (eqv? x -0.0)))) -FlonumNegZero]
    [(~var i (3d (lambda (x) (eqv? x +nan.0)))) -FlonumNan]
    [(~var i (3d(lambda (x) (eqv? x +inf.0)))) (-val +inf.0)]
    [(~var i (3d (lambda (x) (eqv? x -inf.0)))) (-val -inf.0)]
    [(~var i (3d (conjoin flonum? positive?))) -PosFlonum]
    [(~var i (3d (conjoin flonum? negative?))) -NegFlonum]
    [(~var i (3d flonum?)) -Flonum] ; for nan
    [(~var i (3d (lambda (x) (eqv? x 0.0f0)))) -SingleFlonumPosZero]
    [(~var i (3d (lambda (x) (eqv? x -0.0f0)))) -SingleFlonumNegZero]
    [(~var i (3d (lambda (x) (eqv? x +nan.f)))) -SingleFlonumNan]
    [(~var i (3d(lambda (x) (eqv? x +inf.f)))) (-val +inf.f)]
    [(~var i (3d (lambda (x) (eqv? x -inf.f)))) (-val -inf.f)]
    [(~var i (3d (conjoin single-flonum? positive?))) -PosSingleFlonum]
    [(~var i (3d (conjoin single-flonum? negative?))) -NegSingleFlonum]
    [(~var i (3d single-flonum?)) -SingleFlonum] ; for nan
    [(~var i (3d inexact-real?)) -InexactReal] ; catch-all, just in case
    [(~var i (3d real?)) -Real] ; catch-all, just in case
    ;; a complex number can't have a float imaginary part and an exact real part
    [(~var i (3d (conjoin number? exact?)))
     -ExactNumber]
    [(~var i (3d (conjoin number? (lambda (x) (and (flonum? (imag-part x))
                                                   (flonum? (real-part x)))))))
     -FloatComplex]
    [(~var i (3d (conjoin number? (lambda (x) (and (single-flonum? (imag-part x))
                                                   (single-flonum? (real-part x)))))))
     -SingleFlonumComplex]
    ;; can't have real and imaginary parts that are both inexact, but not the same precision
    [(~var i (3d number?)) -Number] ; otherwise, Number
    
    ;; 80-bit flonums
    [(~var i (3d (lambda (x) (eqv? x 0.0t0)))) -ExtFlonumPosZero]
    [(~var i (3d (lambda (x) (eqv? x -0.0t0)))) -ExtFlonumNegZero]
    [(~var i (3d (lambda (x) (eqv? x +nan.t)))) -ExtFlonumNan]
    [(~var i (3d (lambda (x) (eqv? x +inf.t)))) (-val +inf.t)]
    [(~var i (3d (lambda (x) (eqv? x -inf.t)))) (-val -inf.t)]
    [(~var i (3d (conjoin extflonum? (λ (x) (extfl> x 0.0t0))))) -PosExtFlonum]
    [(~var i (3d (conjoin extflonum? (λ (x) (extfl< x 0.0t0))))) -NegExtFlonum]
    [(~var i (3d extflonum?)) -ExtFlonum] ; for nan
    
    [i:str -String]
    [i:char -Char]
    [i:keyword (-val (syntax-e #'i))]
    [i:bytes -Bytes]
    [i:byte-pregexp -Byte-PRegexp]
    [i:byte-regexp -Byte-Regexp]
    [i:pregexp -PRegexp]
    [i:regexp  -Regexp]
    [() -Null]
    [(i . r)
     (match (and expected (resolve (intersect expected (-pair Univ Univ))))
       [(Pair: a-ty d-ty)
        (-pair
         (tc-literal #'i a-ty)
         (tc-literal #'r d-ty))]
       [t 
        (-pair (tc-literal #'i) (tc-literal #'r))])]
    [(~var i (3d vector?))
     (define vec-val (syntax-e #'i))
     (define vec-ty
       (match (and expected (resolve (intersect expected -VectorTop)))
         [(Is-a: (Vector: t))
          (make-Vector
           (check-below
            (apply Un (for/list ([l (in-vector vec-val)])
                        (tc-literal l t)))
            t))]
         [(Is-a: (HeterogeneousVector: ts))
          (make-HeterogeneousVector
           (for/list ([l (in-vector (syntax-e #'i))]
                      [t (in-list/rest ts #f)])
             (cond-check-below (tc-literal l t) t)))]
         [_ (make-HeterogeneousVector (for/list ([l (in-vector (syntax-e #'i))])
                                        (generalize (tc-literal l #f))))]))
     (if (with-refinements?)
         (-refine/fresh v vec-ty (-eq (-lexp (vector-length vec-val))
                                      (-vec-len-of (-id-path v))))
         vec-ty)]
    [(~var i (3d hash?))
     (let ([h (syntax-e #'i)])
       (match (and expected (resolve (intersect expected (-Immutable-HT Univ Univ))))
        [(Immutable-HashTable: k v)
         (let* ([kts (hash-map h (lambda (x y) (tc-literal x k)))]
                [vts (hash-map h (lambda (x y) (tc-literal y v)))]
                [kt (apply Un kts)]
                [vt (apply Un vts)])
           (-Immutable-HT (check-below kt k) (check-below vt v)))]
        [_
         (let* ([kts (hash-map h (lambda (x y) (tc-literal x)))]
                [vts (hash-map h (lambda (x y) (tc-literal y)))]
                [kt (generalize (apply Un kts))]
                [vt (generalize (apply Un vts))])
           (-Immutable-HT kt vt))]))]
    [(~var i (3d prefab-struct-key))
     (tc-prefab (syntax-e #'i) expected)]
    [_ Univ]))

;; Typecheck a prefab struct literal
(define (tc-prefab struct-inst expected)
  (define expected-ts
    (match (and expected (resolve expected))
      [(Prefab: _ ts) (in-list/rest ts #f)]
      [_ (in-cycle (in-value #f))]))
  (define key (prefab-struct-key struct-inst))
  (define struct-vec (struct->vector struct-inst))
  (define fields
    (for/list ([elem (in-vector struct-vec 1)]
               [expected-t expected-ts])
      (tc-literal elem expected-t)))
  (make-Prefab (normalize-prefab-key key (length fields))
               fields))
