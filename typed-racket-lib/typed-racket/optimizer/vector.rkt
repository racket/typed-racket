#lang racket/base

(require syntax/parse
         racket/match racket/flonum racket/extflonum
         (for-template racket/base racket/flonum racket/extflonum racket/unsafe/ops)
         "../utils/utils.rkt"
         (rep type-rep)
         (types type-table utils numeric-tower abbrev)
         (optimizer utils logging fixnum))

(provide vector-opt-expr)

(define-literal-syntax-class vector-length-proc
  (vector-length unsafe-vector-length unsafe-vector*-length))

(define-literal-syntax-class vector-length)
(define-literal-syntax-class vector-ref)
(define-literal-syntax-class vector-set!)
(define-literal-syntax-class flvector-length)
(define-unsafe-syntax-class flvector-ref)
(define-unsafe-syntax-class flvector-set!)
(define-literal-syntax-class extflvector-length)
(define-unsafe-syntax-class extflvector-ref)
(define-unsafe-syntax-class extflvector-set!)

(define-syntax-class vector-op
  #:attributes (unsafe unsafe-no-impersonator check-immutable?)
  #:commit
  ;; we need the non-* versions of these unsafe operations to be chaperone-safe
  ;; the `immutable?` attribute tells if we should fall back to the unoptimized version
  ;;   when the value is immutable
  (pattern :vector-ref^
           #:with unsafe #'unsafe-vector-ref
           #:with unsafe-no-impersonator #'unsafe-vector*-ref
           #:attr check-immutable? #false)
  (pattern :vector-set!^ #:with unsafe #'unsafe-vector-set! #:with unsafe-no-impersonator #'unsafe-vector*-set!
           #:attr check-immutable? #true))
(define-merged-syntax-class flvector-op (flvector-ref^ flvector-set!^ extflvector-ref^ extflvector-set!^))

(define-syntax-class known-length-vector-expr
  #:commit
  #:attributes (len opt)
  (pattern (~and e :opt-expr)
    #:attr tys (match (type-of #'e)
                 [(tc-result1: (HeterogeneousVector: tys)) tys]
                 [_ #f])
    #:when (attribute tys)
    #:attr len (length (attribute tys))))

(define-syntax-class vector-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  #:attributes (opt)
  ;; vector-length of a known-length vector
  (pattern (#%plain-app op:vector-length-proc v:known-length-vector-expr)
    #:do [(log-opt "known-length vector-length" "Static vector length computation.")]
    ; v may have side effects
    #:with opt #`(let () v.opt #,(attribute v.len)))
  ;; optimize vector-length on all vectors.
  ;; since the program typechecked, we know the arg is a vector.
  ;; we can optimize no matter what.
  (pattern (#%plain-app op:vector-length^ v:opt-expr)
    #:do [(log-opt "vector-length" "Vector check elimination.")]
    #:with opt #'(unsafe-vector-length v.opt))
  ;; same for flvector-length and unsafe-flvector-length
  (pattern (#%plain-app op:flvector-length^ v:opt-expr)
    #:do [(log-opt "flvector-length" "Float vector check elimination.")]
    #:with opt #'(unsafe-flvector-length v.opt))
  (pattern (#%plain-app op:extflvector-length^ v:opt-expr)
    #:do [(log-opt "extflvector-length" "Extflonum vector check elimination.")]
    #:with opt #'(unsafe-extflvector-length v.opt))
  ;; we can optimize vector ref and set! on vectors of known length if we know
  ;; the index is within bounds (for now, literal or singleton type)
  (pattern (#%plain-app op:vector-op v:known-length-vector-expr i:value-expr new:opt-expr ...)
    #:when (<= 0 (attribute i.val) (sub1 (attribute v.len)))
    #:do [(log-opt "vector" "Vector bounds checking elimination.")]
    #:with opt #`(let ([new-v v.opt])
                   #,(if (attribute op.check-immutable?)
                         #`(when (immutable? new-v)
                             (op new-v i.opt new.opt ...)) ; produces the correct error message
                         #'(begin))
                   (op.unsafe new-v i.opt new.opt ...)))

  ;; we can do the bounds checking separately, to eliminate some of the checks
  (pattern (#%plain-app op:vector-op v:opt-expr i:fixnum-expr new:opt-expr ...)
    #:do [(log-opt "vector partial bounds checking elimination"
                   "Partial bounds checking elimination.")]
    #:with opt
      (let ([safe-fallback #'(op new-v new-i new.opt ...)]
            [i-known-nonneg? (subtypeof? #'i -NonNegFixnum)])
        #`(let ([new-i i.opt]
                [new-v v.opt])
            #,(if (attribute op.check-immutable?)
                  #`(when (immutable? new-v)
                      #,safe-fallback) ; produces the correct error message
                  #'(begin))
            ;; do the impersonator check up front, to avoid doing it twice (length and op)
            (if (impersonator? new-v)
                (if #,(let ([one-sided #'(unsafe-fx< new-i (unsafe-vector-length new-v))])
                        (if i-known-nonneg?
                            ;; we know it's nonnegative, one-sided check
                            one-sided
                            #`(and (unsafe-fx>= new-i 0)
                                   #,one-sided)))
                    (op.unsafe new-v new-i new.opt ...)
                    #,safe-fallback) ; will error. to give the right error message
                ;; not an impersonator, can use unsafe-vector* ops
                (if #,(let ([one-sided #'(unsafe-fx< new-i (unsafe-vector*-length new-v))])
                        (if i-known-nonneg?
                            one-sided
                            #`(and (unsafe-fx>= new-i 0)
                                   #,one-sided)))
                    (op.unsafe-no-impersonator new-v new-i new.opt ...)
                    #,safe-fallback)))))
  ;; similarly for flvectors and extflvectors
  (pattern (#%plain-app op:flvector-op v:opt-expr i:fixnum-expr new:opt-expr ...)
    #:do [(define flvector? (subtypeof? #'v -FlVector))
          (log-opt (format "~a partial bounds checking elimination"
                           (if flvector? "flvector" "extflvector"))
                   "Partial bounds checking elimination.")]
    #:with opt
      (let ([safe-fallback #'(op new-v new-i new.opt ...)]
            [i-known-nonneg? (subtypeof? #'i -NonNegFixnum)])
        #`(let ([new-i i.opt]
                [new-v v.opt])
            (if #,(let ([one-sided #`(unsafe-fx< new-i (#,(if flvector?
                                                              #'unsafe-flvector-length
                                                              #'unsafe-extflvector-length)
                                                        new-v))])
                    (if i-known-nonneg?
                        one-sided
                        #`(and (unsafe-fx>= new-i 0)
                               #,one-sided)))
                (op.unsafe new-v new-i new.opt ...)
                #,safe-fallback)))))
