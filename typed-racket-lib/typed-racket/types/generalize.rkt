#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep)
         "abbrev.rkt" "subtype.rkt" "substitute.rkt"
         "numeric-tower.rkt"
         racket/match)

(provide generalize)

;; used to produce a more general type for loop variables, vectors, etc.
;; generalize : Type -> Type
(define (generalize t)
  (let/ec exit
    (let loop ([t* t])
      (match t*
        [(== -Null) (-lst Univ)]
        [(== -Zero) -Int]
        [(List: ts) (-lst (apply Un ts))]
        [(? (lambda (t) (subtype t -Bottom))) Univ]
        [(? (lambda (t) (subtype t -Int))) -Int]
        [(? (lambda (t) (subtype t -Rat))) -Rat]
        [(? (lambda (t) (subtype t -Flonum))) -Flonum]
        [(? (lambda (t) (subtype t -SingleFlonum))) -SingleFlonum]
        [(? (lambda (t) (subtype t -InexactReal))) -InexactReal]
        [(? (lambda (t) (subtype t -Real))) -Real]
        [(? (lambda (t) (subtype t -ExactNumber))) -ExactNumber]
        [(? (lambda (t) (subtype t -FloatComplex))) -FloatComplex]
        [(? (lambda (t) (subtype t -SingleFlonumComplex))) -SingleFlonumComplex]
        [(? (lambda (t) (subtype t -Number))) -Number]
        [(? (lambda (t) (subtype t -Base-Regexp))) -Regexp]
        [(? (lambda (t) (subtype t -PRegexp))) -PRegexp]
        [(? (lambda (t) (subtype t -Byte-Base-Regexp))) -Byte-Regexp]
        [(? (lambda (t) (subtype t -Byte-PRegexp))) -Byte-PRegexp]
        [(? (lambda (t) (subtype t -Char))) -Char]
        [(? (lambda (t) (subtype t -ExtFlonum))) -ExtFlonum]
        [(Listof: _) t*]
        [(Immutable-HeterogeneousVector: ts) (apply -vec* ts)]
        [(Mutable-HeterogeneousVector: ts) (apply -vec* ts)]
        [(Immutable-Vector: t) (-vec t)]
        [(Mutable-Vector: t) (-vec t)]
        [(Immutable-HashTable: k v) (-HT k v)]
        [(Mutable-HashTable: k v) (-HT k v)]
        [(Weak-HashTable: k v) (-HT k v)]
        [(Pair: t1 (== -Null)) (-lst t1)]
        [(MPair: t1 (== -Null)) (-mlst t1)]
        [(or (Pair: t1 t2) (MPair: t1 t2))
         (let ([t-new (loop t2)])
           (define -lst-type
             ((match t*
                [(Pair: _ _) -lst]
                [(MPair: _ _) -mlst])
              t1))
           (if (type-equiv? -lst-type t-new)
               -lst-type
               (exit t)))]
        [(ListDots: t bound) (-lst (substitute Univ bound t))]
        [(? (lambda (t) (subtype t -Symbol))) -Symbol]
        [(== -True) -Boolean]
        [(Refine: t _) (loop t)]
        [_ t*]))))
