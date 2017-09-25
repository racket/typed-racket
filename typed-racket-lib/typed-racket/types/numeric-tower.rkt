#lang racket/base

(require "../utils/utils.rkt"
         racket/match
         (rep core-rep type-rep rep-utils type-mask
              numeric-base-types base-union base-type-rep
              object-rep prop-rep)
         (types numeric-predicates)
         ;; For base type contracts
         (for-template racket/base racket/contract/base (types numeric-predicates)))

(provide portable-fixnum? portable-index?
         -Zero -One -PosByte -Byte -PosIndex -Index
         -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum -Fixnum
         -PosInt -Nat -NegInt -NonPosInt -Int
         -PosRat -NonNegRat -NegRat -NonPosRat -Rat
         -FlonumPosZero -FlonumNegZero -FlonumZero -FlonumNan
         -PosFlonum -NonNegFlonum -NegFlonum -NonPosFlonum -Flonum
         -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero
         -SingleFlonumNan -PosSingleFlonum -NonNegSingleFlonum -NegSingleFlonum
         -NonPosSingleFlonum -SingleFlonum
         -InexactRealPosZero -InexactRealNegZero -InexactRealZero -InexactRealNan
         -PosInexactReal -NonNegInexactReal -NegInexactReal -NonPosInexactReal -InexactReal
         -RealZero -RealZeroNoNan -PosReal -NonNegReal -NegReal -NonPosReal -Real
         -PosInfinity -NegInfinity
         -ExactImaginary -FloatImaginary -SingleFlonumImaginary -InexactImaginary -Imaginary
         -ExactNumber -ExactComplex -FloatComplex -SingleFlonumComplex -InexactComplex -Number
         has-int-provable-range?
         int-type->provable-range
         extract-props
         (rename-out (-Int -Integer)))

;;
;; unions of numeric bits defined below
;;


;; Infinities (These are part of Flonum/Single-Flonum, but useful abbreviatios.)
(define/decl -PosInfinity (Un (make-Value +inf.0) (make-Value +inf.f)))
(define/decl -NegInfinity (Un (make-Value -inf.0) (make-Value -inf.f)))


;; Integers
(define/decl -PosByte (Un -One -Byte>1))
(define/decl -Byte    (Un -Zero -PosByte))

(define/decl -PosIndex    (Un -One -Byte>1 -PosIndexNotByte))
(define/decl -Index       (Un -Zero -PosIndex))

(define/decl -PosFixnum    (Un -PosFixnumNotIndex -PosIndex))
(define/decl -NonNegFixnum (Un -PosFixnum -Zero))

(define/decl -NonPosFixnum (Un -NegFixnum -Zero))
(define/decl -Fixnum       (Un -NegFixnum -Zero -PosFixnum))

(define/decl -PosInt    (Un -PosIntNotFixnum -PosFixnum))
(define/decl -NonNegInt (Un -PosInt -Zero))
(define/decl -Nat -NonNegInt)

(define/decl -NegInt    (Un -NegIntNotFixnum -NegFixnum))
(define/decl -NonPosInt (Un -NegInt -Zero))
(define/decl -Int       (Un -NegInt -Zero -PosInt))

(define/decl -PosRat    (Un -PosRatNotInt -PosInt))
(define/decl -NonNegRat (Un -PosRat -Zero))

(define/decl -NegRat    (Un -NegRatNotInt -NegInt))
(define/decl -NonPosRat (Un -NegRat -Zero))
(define/decl -Rat       (Un -NegRat -Zero -PosRat))

(define/decl -FlonumZero (Un -FlonumPosZero -FlonumNegZero -FlonumNan))
(define/decl -PosFlonum (Un -PosFlonumNoNan -FlonumNan))
(define/decl -NonNegFlonum (Un -PosFlonum -FlonumZero))
(define/decl -NegFlonum (Un -NegFlonumNoNan -FlonumNan))
(define/decl -NonPosFlonum (Un -NegFlonum -FlonumZero))
(define/decl -Flonum (Un -NegFlonumNoNan -FlonumNegZero -FlonumPosZero -PosFlonumNoNan -FlonumNan))

(define/decl -SingleFlonumZero (Un -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumNan))
(define/decl -InexactRealNan     (Un -FlonumNan -SingleFlonumNan))
(define/decl -InexactRealPosZero (Un -SingleFlonumPosZero -FlonumPosZero))
(define/decl -InexactRealNegZero (Un -SingleFlonumNegZero -FlonumNegZero))
(define/decl -InexactRealZero    (Un -InexactRealPosZero
                                 -InexactRealNegZero
                                 -InexactRealNan))


(define/decl -PosSingleFlonum    (Un -PosSingleFlonumNoNan -SingleFlonumNan))
(define/decl -PosInexactReal     (Un -PosSingleFlonum -PosFlonum))
(define/decl -NonNegSingleFlonum (Un -PosSingleFlonum -SingleFlonumZero))
(define/decl -NonNegInexactReal  (Un -PosInexactReal -InexactRealZero))

(define/decl -NegSingleFlonum    (Un -NegSingleFlonumNoNan -SingleFlonumNan))
(define/decl -NegInexactReal     (Un -NegSingleFlonum -NegFlonum))
(define/decl -NonPosSingleFlonum (Un -NegSingleFlonum -SingleFlonumZero))
(define/decl -NonPosInexactReal  (Un -NegInexactReal -InexactRealZero))
(define/decl -SingleFlonum       (Un -NegSingleFlonum -SingleFlonumNegZero -SingleFlonumPosZero -PosSingleFlonum -SingleFlonumNan))
(define/decl -InexactReal        (Un -SingleFlonum -Flonum))

;; Reals
(define/decl -RealZero      (Un -Zero -InexactRealZero))
(define/decl -RealZeroNoNan (Un -Zero -InexactRealPosZero -InexactRealNegZero))
(define/decl -PosReal       (Un -PosRat -PosInexactReal))
(define/decl -NonNegReal    (Un -NonNegRat -NonNegInexactReal))
(define/decl -NegReal       (Un -NegRat -NegInexactReal))
(define/decl -NonPosReal    (Un -NonPosRat -NonPosInexactReal))
(define/decl -Real          (Un -Rat -InexactReal))

(define/decl -ExactNumber (Un -ExactImaginary -ExactComplex -Rat))
(define/decl -InexactImaginary (Un -FloatImaginary -SingleFlonumImaginary))
(define/decl -Imaginary (Un -ExactImaginary -InexactImaginary))
(define/decl -InexactComplex (Un -FloatComplex -SingleFlonumComplex))
(define/decl -Complex (Un -Real -Imaginary -ExactComplex -InexactComplex))
(define/decl -Number -Complex)


;; i.e. if we can prove an int is in this range,
;; then it is of this type
(define provable-bounds-hash
  ;; relies on numeric bits being eq?
  (hasheq (Base-bits -Zero) (cons 0 0)
          (Base-bits -One) (cons 1 1)
          (BaseUnion-nbits -Byte) (cons 0 255)
          (BaseUnion-nbits -PosByte) (cons 1 255)
          (Base-bits -Byte>1) (cons 2 255)
          (BaseUnion-nbits -PosInt) (cons 1 #f)
          (BaseUnion-nbits -Nat) (cons 0 #f)
          (BaseUnion-nbits -NonPosInt) (cons #f 0)
          (BaseUnion-nbits -NegInt) (cons #f -1)))


(define (has-int-provable-range? t)
  (and (hash-ref provable-bounds-hash t #f) #t))

(define (int-type->provable-range t)
  (match t
    [(Base: _ #t nbits _ _)
     (hash-ref provable-bounds-hash nbits #f)]
    [(BaseUnion: 0 nbits)
     (hash-ref provable-bounds-hash nbits #f)]
    [_ #f]))

;; i.e. if we know an int has this type, we know it has
;; these bounds
(define int-type-bounds-hash
  ;; relies on numeric bits being eq?
  (hasheq (Base-bits -Zero) (cons 0 0)
          (Base-bits -One) (cons 1 1)
          (BaseUnion-nbits -Byte) (cons 0 255)
          (BaseUnion-nbits -PosByte) (cons 1 255)
          (Base-bits -Byte>1) (cons 2 255)
          (BaseUnion-nbits -PosInt) (cons 1 #f)
          (BaseUnion-nbits -Nat) (cons 0 #f)
          (BaseUnion-nbits -NonPosInt) (cons #f 0)
          (BaseUnion-nbits -NegInt) (cons #f -1)
          (BaseUnion-nbits -Index) (cons 0 #f)
          (BaseUnion-nbits -PosIndex) (cons 1 #f)
          (BaseUnion-nbits -PosFixnum) (cons 1 #f)
          (BaseUnion-nbits -NonNegFixnum) (cons 0 #f)
          (BaseUnion-nbits -NonPosFixnum) (cons #f 0)
          (Base-bits -NegFixnum) (cons #f -1)))

(define (int-type->known-bounds t)
  (match t
    [(Base: _ #t nbits _ _)
     (hash-ref int-type-bounds-hash nbits #f)]
    [(BaseUnion: 0 nbits)
     (hash-ref int-type-bounds-hash nbits #f)]
    [_ #f]))


(define all-bounded-int-types
  ;; relies on numeric bits being eq?
  (for/hasheq ([t (in-list (list -Zero -One -PosByte -Byte>1 -Byte -PosIndex -Index
                                 -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum
                                 -PosInt -Nat -NegInt -NonPosInt))])
    (values (if (BaseUnion? t)
                (BaseUnion-nbits t)
                (Base-bits t))
            #t)))

(define (bounded-int-type? t)
  (match t
    [(Base: _ #t nbits _ _)
     (hash-ref all-bounded-int-types nbits #f)]
    [(BaseUnion: 0 nbits)
     (hash-ref all-bounded-int-types nbits #f)]
    [_ #f]))


;; extract-props : Object Type -> (values Type (listof Prop?))
;; given the fact that 'obj' is of type 'type',
;; look inside of type trying to learn
;; more info about obj
;;
;; NOTE: why is this HERE? It depends on the numeric
;; types defined in this file... so it needs to be
;; here or further downstream. At a glance I couldn't
;; see an ideal place to stick it, so here it landed -AMK
(define (extract-props obj type)
  (cond
    [(Empty? obj) (values type '())]
    [else
     (define props '())
     (define new-type
       (let extract ([rep type]
                     [obj obj])
         (match rep
           [(app int-type->known-bounds
                 (cons maybe-lower-bound maybe-upper-bound))
            #:when (with-refinements?)
            (when maybe-lower-bound
              (set! props (cons (-leq (-lexp maybe-lower-bound) (-lexp obj))
                                props)))
            (when maybe-upper-bound
              (set! props (cons (-leq (-lexp obj) (-lexp maybe-upper-bound))
                                props)))
            rep]
           [(Pair: t1 t2) (make-Pair (extract t1 (-car-of obj))
                                     (extract t2 (-cdr-of obj)))]
           [(Refine-obj: obj t prop)
            (set! props (cons prop props))
            (extract t obj)]
           [(HeterogeneousVector: ts)
            #:when (with-refinements?)
            (set! props (cons (-eq (-vec-len-of obj) (-lexp (length ts)))
                              props))
            rep]
           [(Intersection: ts _)
            (apply -unsafe-intersect
                   (for/list ([t (in-list ts)])
                     (extract t obj)))]
           [_ rep])))
     (values new-type props)]))
