#lang racket/base

;; This file contains the definitions for Base types that are not numeric
;; (i.e. where number? returns #f for values of the type)

(require "../utils/utils.rkt"
         (rep rep-utils base-type-rep type-mask core-rep)
         racket/undefined
         racket/unsafe/undefined
         (types numeric-predicates)
         racket/extflonum
         ;; for base type contracts and predicates
         ;; use '#%place to avoid the other dependencies of `racket/place`
         (for-template
           racket/base
           racket/contract/base
           racket/undefined
           racket/unsafe/undefined
           racket/extflonum
           (only-in racket/pretty pretty-print-style-table?)
           (only-in racket/udp udp?)
           (only-in racket/tcp tcp-listener?)
           (only-in racket/flonum flvector?)
           (only-in racket/extflonum extflvector?)
           (only-in racket/fixnum fxvector?)
           (only-in racket/future fsemaphore?)
           (only-in '#%place place? place-channel?))
         (only-in racket/pretty pretty-print-style-table?)
         (only-in racket/udp udp?)
         (only-in racket/tcp tcp-listener?)
         (only-in racket/flonum flvector?)
         (only-in racket/extflonum extflvector?)
         (only-in racket/fixnum fxvector?)
         (only-in racket/future fsemaphore?)
         (only-in '#%place place? place-channel?))

(provide bbits->base-types
         bbits->atom?
         bbits-subset?
         bbits-overlap?
         bbits-union
         bbits-intersect
         bbits-subtract)

;; these logical combinators are for single argument 
;; functions and perform better than the generic
;; variants from racket/function
(define-syntax-rule (¬ f) (λ (x) (not (f x))))
(define-syntax-rule (compose/and f ...) (λ (x) (and (f x) ...)))

;; returns the single non-numeric Base type represented
;; represented by bits, or #f if it is #b0 or more than
;; one bit is set
(define (bbits->atom? bits)
  (hash-ref base-atom-hash bits #f))

;; takes the bitwise representation of a union of non-numeric
;; Base types and returns a list of the present Base types
(define (bbits->base-types bbits)
  (cond
    [(eqv? 0 bbits) '()]
    [else
     (for*/fold ([acc '()])
                ([low (in-range 0 base-count 8)]
                 [high (in-value (min (+ low 8) base-count))]
                 #:when (not (zero? (bitwise-bit-field bbits low high))))
       (for/fold ([acc acc])
                 ([idx (in-range low high)]
                  #:when (bitwise-bit-set? bbits idx))
         (cons (vector-ref base-atom-vector idx) acc)))]))

;; bitwise set operations
;;
;; Note that for for non-numeric Base bits we assume they
;; can be up to 62 bits (see declarations below), so we use
;; 'bitwise' operations since on 32-bit machines they are
;; not guaranteed to be fixnums.

(define (bbits-subset? bbits1 bbits2)
  (eqv? 0 (bbits-subtract bbits1 bbits2)))

(define (bbits-overlap? bbits1 bbits2)
  (not (eqv? 0 (bitwise-and bbits1 bbits2))))

(define (bbits-union bbits1 bbits2)
  (bitwise-ior bbits1 bbits2))

(define (bbits-intersect bbits1 bbits2)
  (bitwise-and bbits1 bbits2))

(define (bbits-subtract bbits1 bbits2)
  (bitwise-and bbits1 (bitwise-not bbits2)))

(define-base-types
  #:numeric? #f
  ;; 62 bits is the max for a 2's complement 64-bit fixnum
  #:max-count 62
  #:count base-count
  #:atom-vector base-atom-vector
  #:atom-hash base-atom-hash
  #:atoms
  [-False False #'not not]
  [-True True #'(λ (x) (eq? #t x)) (λ (x) (eq? #t x))]
  [-Null Null #'null? null?]
  [-Void Void #'void? void?]
  [-Char Char #'char? char?]
  [-Symbol Symbol #'symbol? symbol?]
  [-String String #'string? string?]
  [-Output-Port Output-Port #'output-port? output-port?]
  [-Input-Port Input-Port #'input-port? input-port?]
  [-Bytes Bytes #'bytes? bytes?]
  [-Base-Regexp
   Base-Regexp
   #'(and/c regexp? (not/c pregexp?))
   (compose/and regexp? (¬ pregexp?))]
  [-PRegexp PRegexp #'pregexp? pregexp?]
  [-Byte-Base-Regexp Byte-Base-Regexp
                     #'(and/c byte-regexp? (not/c byte-pregexp?))
                     (compose/and byte-regexp? (¬ byte-pregexp?))]
  [-Byte-PRegexp Byte-PRegexp #'byte-pregexp? byte-pregexp?]
  [-Keyword Keyword #'keyword? keyword?]
  [-Thread Thread #'thread? thread?]
  [-Path Path #'path? path?]
  [-Resolved-Module-Path
   Resolved-Module-Path
   #'resolved-module-path?
   resolved-module-path?]
  [-Module-Path-Index
   Module-Path-Index
   #'module-path-index?
   module-path-index?]
  [-OtherSystemPath
   OtherSystemPath
   #'(and/c path-for-some-system? (not/c path?))
   (compose/and path-for-some-system? (¬ path?))]
  [-Cont-Mark-Set
   Continuation-Mark-Set
   #'continuation-mark-set?
   continuation-mark-set?]
  [-TCP-Listener TCP-Listener #'tcp-listener? tcp-listener?]
  [-UDP-Socket UDP-Socket #'udp? udp?]
  [-FlVector FlVector #'flvector? flvector?]
  [-FxVector FxVector #'fxvector? fxvector?]
  [-Namespace Namespace #'namespace? namespace?]
  [-Compiled-Module-Expression
   Compiled-Module-Expression
   #'compiled-module-expression?
   compiled-module-expression?]
  [-Compiled-Non-Module-Expression
   Compiled-Non-Module-Expression
   #'(and/c compiled-expression?
            (not/c  compiled-module-expression?))
   (compose/and compiled-expression?
                (¬ compiled-module-expression?))]
  [-Pretty-Print-Style-Table
   Pretty-Print-Style-Table
   #'pretty-print-style-table?
   pretty-print-style-table?]
  [-Read-Table Read-Table #'readtable? readtable?]
  [-Special-Comment Special-Comment #'special-comment? special-comment?]
  [-Custodian Custodian #'custodian? custodian?]
  [-Parameterization Parameterization #'parameterization? parameterization?]
  [-Inspector Inspector #'inspector? inspector?]
  [-Namespace-Anchor Namespace-Anchor #'namespace-anchor? namespace-anchor?]
  [-Variable-Reference Variable-Reference #'variable-reference? variable-reference?]
  [-Internal-Definition-Context
   Internal-Definition-Context
   #'internal-definition-context?
   internal-definition-context?]
  [-Subprocess Subprocess #'subprocess? subprocess?]
  [-Security-Guard Security-Guard #'security-guard? security-guard?]
  [-Thread-Group Thread-Group #'thread-group? thread-group?]
  [-Struct-Type-Property Struct-Type-Property #'struct-type-property? struct-type-property?]
  [-Impersonator-Property Impersonator-Property #'impersonator-property? impersonator-property?]
  [-Semaphore Semaphore #'semaphore? semaphore?]
  [-FSemaphore FSemaphore #'fsemaphore? fsemaphore?]
  [-Bytes-Converter Bytes-Converter #'bytes-converter? bytes-converter?]
  [-Pseudo-Random-Generator
   Pseudo-Random-Generator
   #'pseudo-random-generator?
   pseudo-random-generator?]
  [-Logger Logger #'logger? logger?]
  [-Log-Receiver Log-Receiver #'log-receiver? log-receiver?]
  [-Place Place #'place? place?]
  [-Base-Place-Channel
   Base-Place-Channel #'(and/c place-channel? (not/c place?))
   (compose/and place-channel? (¬ place?))]
  [-Will-Executor Will-Executor #'will-executor? will-executor?]
  [-Environment-Variables
   Environment-Variables
   #'environment-variables?
   environment-variables?]
  [-Undefined
   Undefined
   #'(λ (x) (eq? x undefined))
   (λ (x) (eq? x undefined))]
  [-Unsafe-Undefined
   Unsafe-Undefined
   #'(λ (x) (eq? x unsafe-undefined))
   (λ (x) (eq? x unsafe-undefined))]
  [-ExtFlVector ExtFlVector #'extflvector? extflvector?]
  ;; 80-bit floating-point numbers
  ;; +nan.t is included in all 80-bit floating-point types
  ;; NOTE: these are here and not in the numeric tower because
  ;; they return #f for number?
  [-ExtFlonumNan
   ExtFlonum-Nan
   #'(and/c extflonum? (lambda (x) (eqv? x +nan.t)))
   (λ (x) (and (extflonum? x) (eqv? x +nan.t)))]
  [-ExtFlonumPosZero
   ExtFlonum-Positive-Zero
   #'(λ (x) (eqv? x 0.0t0))
   (λ (x) (eqv? x 0.0t0))]
  [-ExtFlonumNegZero
   ExtFlonum-Negative-Zero
   #'(λ (x) (eqv? x -0.0t0))
   (λ (x) (eqv? x -0.0t0))]
  [-NegExtFlonumNoNan
   Negative-ExtFlonum-No-NaN
   #'(and/c extflonum? (λ (x) (extfl<= x 0.0t0)))
   (λ (x) (and (extflonum? x) (extfl<= x 0.0t0)))]
  [-PosExtFlonumNoNan
   Positive-ExtFlonum-No-NaN
   #'(and/c extflonum? (λ (x) (extfl>= x 0.0t0)))
   (λ (x) (and (extflonum? x) (extfl>= x 0.0t0)))]
  [-Dead-Code Dead-Code #'(make-none/c 'dead-code/c) (λ (v) #f)])
