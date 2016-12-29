#lang racket/base

(require "../utils/utils.rkt"
         (rep rep-utils type-mask numeric-base-types)
         (types numeric-predicates)
         (only-in (rep type-rep) Un make-Value)
         ;; For base type contracts
         (for-template racket/base racket/contract/base (types numeric-predicates)))

(provide portable-fixnum? portable-index?
         -Zero -One -PosByte -Byte -PosIndex -Index
         -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum -Fixnum
         -PosInt -Nat -NegInt -NonPosInt -Int
         -PosRat -NonNegRat -NegRat -NonPosRat -Rat
         -FlonumPosZero -FlonumNegZero -FlonumZero -FlonumNan -PosFlonum -NonNegFlonum -NegFlonum -NonPosFlonum -Flonum
         -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero -SingleFlonumNan -PosSingleFlonum -NonNegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum -SingleFlonum
         -InexactRealPosZero -InexactRealNegZero -InexactRealZero -InexactRealNan -PosInexactReal -NonNegInexactReal -NegInexactReal -NonPosInexactReal -InexactReal
         -RealZero -RealZeroNoNan -PosReal -NonNegReal -NegReal -NonPosReal -Real
         -PosInfinity -NegInfinity
         -ExactImaginary -FloatImaginary -SingleFlonumImaginary -InexactImaginary -Imaginary
         -ExactNumber -ExactComplex -FloatComplex -SingleFlonumComplex -InexactComplex -Number
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
