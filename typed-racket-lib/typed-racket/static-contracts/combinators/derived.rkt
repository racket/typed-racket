#lang racket/base

;; Static contracts for common data types.
;; These are used during optimizations as simplifications.
;; Ex: (listof/sc any/sc) => list?/sc

(require (for-template racket/async-channel
                       racket/base
                       racket/class
                       racket/future
                       racket/list
                       racket/promise
                       racket/set
                       racket/treelist
                       racket/unit)
         "simple.rkt"
         "structural.rkt")
(provide (all-defined-out))

(define identifier?/sc (flat/sc #'identifier?))
(define box?/sc (flat/sc #'box?))
(define weak-box?/sc (flat/sc #'weak-box?))
(define syntax?/sc (flat/sc #'syntax?))
(define promise?/sc (flat/sc #'promise?))

(define cons?/sc (flat/sc #'cons?))
(define list?/sc (flat/sc #'list?))

(define mpair?/sc (flat/sc #'mpair?))

(define set?/sc (flat/sc #'(lambda (x) (or (set? x) (set-mutable? x) (set-weak? x)))))
(define empty-set/sc (and/sc set?/sc (flat/sc #'set-empty?)))

;; TODO: Split TreeList into Mutable and Immutable
(define treelist?/sc (flat/sc #'treelist?))
(define empty-treelist/sc (and/sc treelist?/sc (flat/sc #'treelist-empty?)))

(define vector?/sc (flat/sc #'vector?))
(define immutable-vector?/sc (and/sc vector?/sc
                                     (flat/sc #'immutable?)))
(define mutable-vector?/sc (and/sc vector?/sc
                                   (flat/sc #'(λ (v) (not (immutable? v))))))

(define hash?/sc (flat/sc #'hash?))
(define immutable-hash?/sc (and/sc hash?/sc (flat/sc #'immutable?)))
(define mutable-hash?/sc (and/sc hash?/sc
                                 (flat/sc #'(λ (h) (not (immutable? h))))
                                 (flat/sc #'(λ (h) (not (hash-weak? h))))))
(define weak-hash?/sc (and/sc hash?/sc (flat/sc #'hash-weak?)))
(define empty-hash/sc (and/sc hash?/sc (flat/sc #'(λ (h) (zero? (hash-count h))))))

(define channel?/sc (flat/sc #'channel?))
(define async-channel?/sc (flat/sc #'async-channel?))
(define thread-cell?/sc (flat/sc #'thread-cell?))
(define prompt-tag?/sc (flat/sc #'continuation-prompt-tag?))
(define continuation-mark-key?/sc (flat/sc #'continuation-mark-key?))
(define sequence?/sc (flat/sc #'sequence?))
(define evt?/sc (flat/sc #'evt?))
(define parameter?/sc (flat/sc #'parameter?))
(define ephemeron?/sc (flat/sc #'ephemeron?))
(define future?/sc (flat/sc #'future?))
(define procedure?/sc (flat/sc #'procedure?))

(define class?/sc (flat/sc #'class?))
(define unit?/sc (flat/sc #'unit?))

(define struct-type?/sc (flat/sc #'struct-type?))
(define struct-type-property?/sc (flat/sc #'struct-type-property?))

