#lang racket/base

;; This file contains the definitions for Base types that are numeric
;; (i.e. where number? returns #t for values of the type)


(require "../utils/utils.rkt"
         (rep rep-utils base-type-rep type-mask core-rep)
         (types numeric-predicates)
         racket/unsafe/ops
         ;; For base type contracts
         (for-template racket/base racket/contract/base (types numeric-predicates)))

(provide portable-fixnum?
         portable-index?
         nbits->atom?
         nbits->base-types
         nbits-subset?
         nbits-overlap?
         nbits-intersect
         nbits-union
         nbits-subtract)

;; Is the number a fixnum on *all* the platforms Racket supports?  This
;; works because Racket compiles only on 32+ bit systems.  This check is
;; done at compile time to typecheck literals -- so use it instead of
;; `fixnum?' to avoid creating platform-dependent .zo files.
(define (portable-fixnum? n)
  (and (exact-integer? n)
       (< n (expt 2 30))
       (>= n (- (expt 2 30)))))
;; same, for indexes
(define (portable-index? n)
  (and (exact-integer? n)
       (< n (expt 2 28))
       (>= n 0)))


;; returns the single numeric Base type represented
;; represented by bits, or #f if it is #b0 or more than
;; one bit is set
(define (nbits->atom? bits)
  (hash-ref numeric-atom-hash bits #f))


;; bitwise set operations
;;
;; Note that for numeric Base bits we assume they can be up
;; to 30 bits (see declarations below), so we use 'unsafe-fx'
;; operations since even on 32-bit machines they are all fixnums.


(define (nbits-subset? nbits1 nbits2)
  (unsafe-fx= 0 (nbits-subtract nbits1 nbits2)))

(define (nbits-overlap? nbits1 nbits2)
  (not (unsafe-fx= 0 (unsafe-fxand nbits1 nbits2))))

(define (nbits-union nbits1 nbits2)
  (unsafe-fxior nbits1 nbits2))

(define (nbits-intersect nbits1 nbits2)
  (unsafe-fxand nbits1 nbits2))

(define (nbits-subtract nbits1 nbits2)
  (unsafe-fxand nbits1 (unsafe-fxnot nbits2)))

;; takes the bitwise representation of a union of numeric Base types
;; and returns a list of the present Base types
(define (nbits->base-types nbits)
  (cond
    [(eqv? 0 nbits) '()]
    [else
     (for*/fold ([acc '()])
                ([low (in-range 0 numeric-count 8)]
                 [high (in-value (min (+ low 8) numeric-count))]
                 #:when (not (zero? (bitwise-bit-field nbits low high))))
       (for/fold ([acc acc])
                 ([idx (in-range low high)]
                  #:when (bitwise-bit-set? nbits idx))
         (cons (vector-ref numeric-atom-vector idx) acc)))]))


(define-base-types
  #:numeric? #t
  ;; 30 bits is the max for a 2's complement 32-bit fixnum
  ;; (since the numeric tower requires < 30 bits, we can
  ;;  make that the max and use unsafe-fx ops for bit computations)
  #:max-count 30
  #:count numeric-count
  #:atom-vector numeric-atom-vector
  #:atom-hash numeric-atom-hash
  #:atoms
  [-Zero Zero #'(λ (n) (eq? n 0)) (λ (n) (eq? n 0))]
  [-One One #'(λ (n) (eq? n 1)) (λ (n) (eq? n 1))]
  [-Byte>1
   Byte-Larger-Than-One
   #'(λ (n) (and (byte? n) (> n 1)))
   (λ (n) (and (byte? n) (> n 1)))]
  [-PosIndexNotByte
   Positive-Index-Not-Byte
   #'(and/c index? positive? (not/c byte?))
   (λ (x) (and (portable-index? x)
               (positive? x)
               (not (byte? x))))]
  [-PosFixnumNotIndex
   Positive-Fixnum-Not-Index
   #'(and/c fixnum? positive? (not/c index?))
   (λ (x) (and (portable-fixnum? x)
               (positive? x)
               (not (portable-index? x))))]
  [-NegFixnum
   Negative-Fixnum
   #'(and/c fixnum? negative?)
   (λ (x) (and (portable-fixnum? x)
               (negative? x)))]
  [-PosIntNotFixnum
   Positive-Integer-Not-Fixnum
   #'(and/c exact-integer? positive? (not/c fixnum?))
   (λ (x) (and (exact-integer? x)
               (positive? x)
               (not (portable-fixnum? x))))]
  [-NegIntNotFixnum
   Negative-Integer-Not-Fixnum
   #'(and/c exact-integer? negative? (not/c fixnum?))
   (λ (x) (and (exact-integer? x)
               (negative? x)
               (not (portable-fixnum? x))))]
  [-PosRatNotInt
   Positive-Rational-Not-Integer
   #'(and/c exact-rational? positive? (not/c integer?))
   (λ (x) (and (exact-rational? x)
               (positive? x)
               (not (exact-integer? x))))]
  [-NegRatNotInt
   Negative-Rational-Not-Integer
   #'(and/c exact-rational? negative? (not/c integer?))
   (λ (x) (and (exact-rational? x)
               (negative? x)
               (not (exact-integer? x))))]
  [-FlonumNan
   Float-Nan
   #'(and/c flonum? (lambda (x) (eqv? x +nan.0)))
   (λ (x) (and (flonum? x) (eqv? x +nan.0)))]
  [-FlonumPosZero
   Float-Positive-Zero
   #'(λ (x) (eqv? x 0.0))
   (λ (x) (eqv? x 0.0))]
  [-FlonumNegZero
   Float-Negative-Zero
   #'(λ (x) (eqv? x -0.0))
   (λ (x) (eqv? x -0.0))]
  [-PosFlonumNoNan
   Positive-Float-No-NaN
   #'(and/c flonum? positive?)
   (λ (x) (and (flonum? x) (positive? x)))]
  [-NegFlonumNoNan
   Negative-Float-No-NaN
   #'(and/c flonum? negative?)
   (λ (x) (and (flonum? x) (negative? x)))]
  [-SingleFlonumNan
   Single-Flonum-Nan
   #'(and/c single-flonum? (lambda (x) (eqv? x +nan.f)))
   (λ (x) (and (single-flonum? x) (eqv? x +nan.f)))]
  [-SingleFlonumPosZero ; disjoint from Flonum 0s
   Single-Flonum-Positive-Zero
   ;; eqv? equates 0.0f0 with itself, but not eq?
   #'(λ (x) (eqv? x 0.0f0))
   (λ (x) (eqv? x 0.0f0))]
  [-SingleFlonumNegZero
   Single-Flonum-Negative-Zero
   #'(λ (x) (eqv? x -0.0f0))
   (λ (x) (eqv? x -0.0f0))]
  [-PosSingleFlonumNoNan
   Positive-Single-Flonum-No-Nan
   #'(and/c single-flonum? positive?)
   (λ (x) (and (single-flonum? x) (positive? x)))]
  [-NegSingleFlonumNoNan
   Negative-Single-Flonum-No-Nan
   #'(and/c single-flonum? negative?)
   (λ (x) (and (single-flonum? x) (negative? x)))]
  [-ExactImaginary
   Exact-Imaginary
   #'(and/c number?
            (not/c real?)
            (λ (x)
              (and
               (eqv? 0 (real-part x))
               (exact? (imag-part x)))))
   (λ (x) (and (number? x)
               (not (real? x))
               (eqv? 0 (real-part x))
               (exact? (imag-part x))))]
  [-ExactComplex
   Exact-Complex
   #'(and/c number?
            (not/c real?)
            (lambda (x)
              (and
               (not (eqv? 0 (real-part x)))
               (exact? (real-part x))
               (exact? (imag-part x)))))
   (λ (x) (and (number? x)
               (not (real? x))
               (not (eqv? 0 (real-part x)))
               (exact? (real-part x))
               (exact? (imag-part x))))]
  [-FloatImaginary
   Float-Imaginary
   #'(and/c number?
            (λ (x)
              (and (flonum? (imag-part x))
                   (eqv? 0 (real-part x)))))
   (λ (x)
     (and (number? x)
          (flonum? (imag-part x))
          (eqv? 0 (real-part x))))]
  [-SingleFlonumImaginary
   Single-Flonum-Imaginary
   #'(and/c number?
            (λ (x)
              (and (single-flonum? (imag-part x))
                   (eqv? 0 (real-part x)))))
   (λ (x)
     (and (number? x)
          (single-flonum? (imag-part x))
          (eqv? 0 (real-part x))))]
  [-FloatComplex
   Float-Complex
   #'(and/c number?
            (lambda (x)
              (and (flonum? (imag-part x))
                   (flonum? (real-part x)))))
   (λ (x)
     (and (number? x)
          (flonum? (imag-part x))
          (flonum? (real-part x))))]
  [-SingleFlonumComplex
   Single-Flonum-Complex
   #'(and/c number?
            (λ (x)
              (and (single-flonum? (imag-part x))
                   (single-flonum? (real-part x)))))
   (λ (x)
     (and (number? x)
          (single-flonum? (imag-part x))
          (single-flonum? (real-part x))))])
