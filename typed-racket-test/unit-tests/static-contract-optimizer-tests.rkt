#lang racket/base

(require "test-utils.rkt"
         racket/list racket/format rackunit
         (static-contracts instantiate optimize combinators structures)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)

(define-syntax (check-optimize stx)
  (syntax-parse stx
    [(_ argument* #:pos positive-expected* #:neg negative-expected*)
     #'(test-suite (~a 'argument*)
         (test-case "Trusted Positive"
           (check-optimize-helper argument* positive-expected* #t #f))
         (test-case "Trusted Negative"
           (check-optimize-helper argument* negative-expected* #f #t)))]))

(define (check-optimize-helper argument expected trusted-positive trusted-negative)
  (define trusted-side
    (cond
      [(and trusted-positive trusted-negative) 'both]
      [trusted-positive 'positive]
      [trusted-negative 'negative]
      [else 'neither]))
  (with-check-info*
    (list (make-check-info 'original argument)
          (make-check-info 'trusted trusted-side)
          (make-check-expected expected))
    (λ ()
      (let ([opt (optimize argument
                   #:trusted-positive trusted-positive
                   #:trusted-negative trusted-negative)])
        (with-check-info* (list (make-check-actual opt))
          (lambda ()
            (unless (equal? opt expected)
              (fail-check))))))))

(define-syntax (check-syntax stx)
  (syntax-parse stx
    [(_ argument* expected*)
     #'(test-case (~a 'argument*)
         (define argument argument*)
         (define expected expected*)
         (with-check-info*
           (list (make-check-info 'original argument)
                 (make-check-expected expected))
           (λ ()
             (let ([ctc (syntax->datum
                         (cadr
                          (instantiate
                            (optimize argument #:trusted-positive #t)
                            (λ (#:reason [reason #f]) (error 'nyi))
                            'impersonator)))])
               (with-check-info* (list (make-check-actual ctc))
                 (λ ()
                   (unless (equal? ctc expected)
                     (fail-check))))))))]))

;; Ids with unique identity so that equals works
(define foo-id #'foo)
(define bar-id #'bar)

(define syntax-tests
  (test-suite "Optimized Syntax Tests"
    (check-syntax list?/sc
      'any/c)
    (check-syntax (arr/sc null #f (list list?/sc))
      '(-> any))
    (check-syntax (hash/sc list?/sc list?/sc)
      '(typed-racket-hash/c list? list?))

    ))


(define optimizer-tests
  (test-suite "Optimizer Tests"
    ;; Lists
    (check-optimize (listof/sc any/sc)
      #:pos any/sc
      #:neg list?/sc)
    (check-optimize (listof/sc none/sc)
      #:pos any/sc
      #:neg empty-list/sc)

    ;; Heterogeneous Lists
    (check-optimize (list/sc any/sc)
      #:pos any/sc
      #:neg (list-length/sc 1))
    (check-optimize (list/sc none/sc)
      #:pos any/sc
      #:neg none/sc)
    (check-optimize (list/sc)
      #:pos any/sc
      #:neg empty-list/sc)

    ;; Sets
    (check-optimize (set/sc any/sc)
      #:pos any/sc
      #:neg set?/sc)
    (check-optimize (set/sc none/sc)
      #:pos any/sc
      #:neg empty-set/sc)


    ;; Vectors
    (let ()
      (define-syntax-rule (make-?vectorof/sc-check ctc flat-ctc)
        (begin
          (check-optimize (ctc any/sc)
            #:pos any/sc
            #:neg flat-ctc)
          (check-optimize (ctc none/sc)
            #:pos (ctc none/sc)
            #:neg (ctc none/sc))))
      (make-?vectorof/sc-check immutable-vectorof/sc immutable-vector?/sc)
      (make-?vectorof/sc-check mutable-vectorof/sc mutable-vector?/sc)
      (make-?vectorof/sc-check vectorof/sc vector?/sc))
    ;; immutable-vectorof is covariant
    (check-optimize (immutable-vectorof/sc (flat/sc #'integer?))
      #:pos any/sc
      #:neg (immutable-vectorof/sc (flat/sc #'integer?)))
    ;; mutable-vectorof and vectorof are invariant
    (check-optimize (mutable-vectorof/sc set?/sc)
      #:pos (mutable-vectorof/sc set?/sc)
      #:neg (mutable-vectorof/sc set?/sc))
    (check-optimize (vectorof/sc (flat/sc #'boolean?))
      #:pos (vectorof/sc (flat/sc #'boolean?))
      #:neg (vectorof/sc (flat/sc #'boolean?)))

    ;; Heterogeneous Vectors
    (check-optimize (vector/sc any/sc)
      #:pos any/sc
      #:neg (vector-length/sc 1))
    (let ()
      (define-syntax-rule (make-?vector/sc-check ctc)
        (check-optimize (ctc none/sc)
          #:pos (ctc none/sc)
          #:neg (ctc none/sc)))
      (make-?vector/sc-check immutable-vector/sc)
      (make-?vector/sc-check mutable-vector/sc)
      (make-?vector/sc-check vector/sc))
    (check-optimize (vector/sc)
      #:pos any/sc
      #:neg empty-vector/sc)
    (check-optimize (vector/sc set?/sc)
      #:pos (vector/sc set?/sc)
      #:neg (vector/sc set?/sc))
    ;; immutable-vector is covariant
    (check-optimize (immutable-vector/sc (flat/sc #'integer?) hash?/sc)
      #:pos any/sc
      #:neg (immutable-vector/sc (flat/sc #'integer?) hash?/sc))
    ;; mutable-vector and vector are invariant
    (check-optimize (mutable-vector/sc list?/sc list?/sc)
      #:pos (mutable-vector/sc list?/sc list?/sc)
      #:neg (mutable-vector/sc list?/sc list?/sc))
    (check-optimize (vector/sc identifier?/sc)
      #:pos (vector/sc identifier?/sc)
      #:neg (vector/sc identifier?/sc))

    ;; HashTables
    (let ()
      (define-syntax-rule (make-?hash/sc-check ctc flat-ctc)
        (begin
          (check-optimize (ctc any/sc any/sc)
            #:pos any/sc
            #:neg flat-ctc)
          (check-optimize (ctc none/sc any/sc)
            #:pos (ctc none/sc any/sc)
            #:neg (ctc none/sc any/sc))
          (check-optimize (ctc any/sc none/sc)
            #:pos (ctc any/sc none/sc)
            #:neg (ctc any/sc none/sc))))
      (make-?hash/sc-check hash/sc hash?/sc)
      (make-?hash/sc-check immutable-hash/sc immutable-hash?/sc)
      (make-?hash/sc-check mutable-hash/sc mutable-hash?/sc)
      (make-?hash/sc-check weak-hash/sc weak-hash?/sc))
    ;; immutable-hash is covariant
    (check-optimize (immutable-hash/sc (flat/sc #'symbol?) (flat/sc #'string?))
      #:pos any/sc
      #:neg (immutable-hash/sc (flat/sc #'symbol?) (flat/sc #'string?)))
    ;; mutable-hash weak-hash hash are invariant
    (check-optimize (mutable-hash/sc identifier?/sc hash?/sc)
      #:pos (mutable-hash/sc identifier?/sc hash?/sc)
      #:neg (mutable-hash/sc identifier?/sc hash?/sc))
    (check-optimize (weak-hash/sc list?/sc (flat/sc #'symbol?))
      #:pos (weak-hash/sc list?/sc (flat/sc #'symbol?))
      #:neg (weak-hash/sc list?/sc (flat/sc #'symbol?)))
    (check-optimize (hash/sc (flat/sc #'symbol?) (flat/sc #'string?))
      #:pos (hash/sc (flat/sc #'symbol?) (flat/sc #'string?))
      #:neg (hash/sc (flat/sc #'symbol?) (flat/sc #'string?)))

    ;; And
    (check-optimize (and/sc set?/sc)
      #:pos any/sc
      #:neg set?/sc)
    (check-optimize (and/sc set?/sc any/sc)
      #:pos any/sc
      #:neg set?/sc)
    (check-optimize (and/sc set?/sc none/sc)
      #:pos any/sc
      #:neg none/sc)
    (check-optimize (and/sc)
      #:pos any/sc
      #:neg any/sc)
    (check-optimize (and/sc any/sc any/sc)
      #:pos any/sc
      #:neg any/sc)


    ;; Or
    (check-optimize (or/sc set?/sc)
      #:pos any/sc
      #:neg set?/sc)
    (check-optimize (or/sc (or/sc set?/sc list?/sc))
      #:pos any/sc
      #:neg (or/sc set?/sc list?/sc))
    (check-optimize (or/sc set?/sc any/sc)
      #:pos any/sc
      #:neg any/sc)
    (check-optimize (or/sc set?/sc none/sc)
      #:pos any/sc
      #:neg set?/sc)
    (check-optimize (or/sc)
      #:pos any/sc
      #:neg none/sc)
    (check-optimize (or/sc none/sc none/sc)
      #:pos any/sc
      #:neg none/sc)
    (check-optimize (or/sc set?/sc (list/sc set?/sc) (list/sc set?/sc set?/sc))
      ;; if all contracts are flat, optimize trusted positive
      #:pos any/sc
      #:neg (or/sc set?/sc (list/sc set?/sc) (list/sc set?/sc set?/sc)))
    (check-optimize (or/sc set?/sc (list/sc (flat/sc #'symbol?)) (box/sc (flat/sc #'symbol?)))
      ;; don't optimize if any contracts are non-flat --- but do optimize under guarded constructors
      #:pos (or/sc set?/sc (list-length/sc 1) (box/sc (flat/sc #'symbol?)))
      #:neg (or/sc set?/sc (list/sc (flat/sc #'symbol?)) (box/sc (flat/sc #'symbol?))))

    ;; None
    (check-optimize none/sc
      #:pos any/sc
      #:neg none/sc)

    ;; Boxes
    (check-optimize (box/sc any/sc)
      #:pos any/sc
      #:neg box?/sc)
    (check-optimize (box/sc none/sc)
      #:pos (box/sc none/sc)
      #:neg (box/sc none/sc))
    (check-optimize (box/sc set?/sc)
      #:pos (box/sc set?/sc)
      #:neg (box/sc set?/sc))

    ;; Syntax Objects
    (check-optimize (syntax/sc any/sc)
      #:pos any/sc
      #:neg syntax?/sc)
    (check-optimize (syntax/sc none/sc)
      #:pos any/sc
      #:neg none/sc)
    (check-optimize (syntax/sc set?/sc)
      #:pos any/sc
      #:neg (syntax/sc set?/sc))

    ;; Promises
    (check-optimize (promise/sc any/sc)
      #:pos any/sc
      #:neg promise?/sc)
    (check-optimize (promise/sc none/sc)
      #:pos any/sc
      #:neg (promise/sc none/sc))
    (check-optimize (promise/sc set?/sc)
      #:pos any/sc
      #:neg (promise/sc set?/sc))
    (check-optimize (promise/sc (box/sc set?/sc))
      #:pos (promise/sc (box/sc set?/sc))
      #:neg (promise/sc (box/sc set?/sc)))

    (check-optimize
      (function/sc #t
                   (list (listof/sc any/sc))
                   (list)
                   (list)
                   (list)
                   #f
                   (list (listof/sc any/sc)))
      #:pos
      (function/sc #t
                   (list list?/sc)
                   (list)
                   (list)
                   (list)
                   #f
                   #f)
      #:neg
      (function/sc #t
                   (list any/sc)
                   (list)
                   (list)
                   (list)
                   #f
                   (list list?/sc)))
    (check-optimize
      (function/sc #t
                   (list (listof/sc any/sc))
                   (list)
                   (list)
                   (list)
                   #f
                   (list any/sc))
      #:pos
      (function/sc #t
                   (list list?/sc)
                   (list)
                   (list)
                   (list)
                   #f
                   #f)
      #:neg
      (function/sc #t
                   (list any/sc)
                   (list)
                   (list)
                   (list)
                   #f
                   (list any/sc)))

    (check-optimize (case->/sc empty)
      #:pos (case->/sc empty)
      #:neg (case->/sc empty))
    (check-optimize (parameter/sc list?/sc set?/sc)
      #:pos (parameter/sc list?/sc any/sc)
      #:neg (parameter/sc any/sc set?/sc))

    (check-optimize
      (case->/sc (list (arr/sc (list (listof/sc any/sc)) (listof/sc (set/sc any/sc)) (list (listof/sc any/sc)))))
      #:pos (case->/sc (list (arr/sc (list list?/sc) (listof/sc set?/sc) #f)))
      #:neg (case->/sc (list (arr/sc (list any/sc) any/sc (list list?/sc)))))

    (check-optimize
      (object/sc #t (list (member-spec 'field 'x (listof/sc any/sc))))
      #:pos (object/sc #t (list (member-spec 'field 'x list?/sc)))
      #:neg (object/sc #t (list (member-spec 'field 'x list?/sc))))

    (check-optimize
      (object/sc #f (list (member-spec 'field 'x (listof/sc any/sc))))
      #:pos (object/sc #f (list (member-spec 'field 'x list?/sc)))
      #:neg (object/sc #f (list (member-spec 'field 'x list?/sc))))

    (check-optimize
      (class/sc #t (list (member-spec 'field 'x (listof/sc any/sc))) null)
      #:pos (class/sc #t (list (member-spec 'field 'x list?/sc)) null)
      #:neg (class/sc #t (list (member-spec 'field 'x list?/sc)) null))

    (check-optimize
      (class/sc #f (list (member-spec 'field 'x (listof/sc any/sc))) null)
      #:pos (class/sc #f (list (member-spec 'field 'x list?/sc)) null)
      #:neg (class/sc #f (list (member-spec 'field 'x list?/sc)) null))

    (check-optimize
      (recursive-sc (list foo-id bar-id)
                    (list (listof/sc (recursive-sc-use foo-id))
                          (listof/sc (recursive-sc-use bar-id)))
                    (recursive-sc-use foo-id))
      #:pos (recursive-sc (list foo-id)
                          (list (listof/sc (recursive-sc-use foo-id)))
                          (recursive-sc-use foo-id))
      #:neg (recursive-sc (list foo-id)
                          (list (listof/sc (recursive-sc-use foo-id)))
                          (recursive-sc-use foo-id)))

    (check-optimize
      (recursive-sc (list foo-id bar-id)
                    (list (listof/sc any/sc )
                          (listof/sc any/sc))
                    (recursive-sc-use foo-id))
      #:pos any/sc
      #:neg list?/sc)

    (check-optimize (cons/sc any/sc list?/sc)
      #:pos any/sc
      #:neg (cons/sc any/sc list?/sc))

    (check-optimize
      (case->/sc
        (list
          (arr/sc empty #f (list set?/sc))
          (arr/sc (list identifier?/sc) #f (list (listof/sc set?/sc)))))
      #:pos  (function/sc #t
                          (list)
                          (list identifier?/sc)
                          (list)
                          (list)
                          #f
                          #f)
      #:neg (case->/sc
              (list
                (arr/sc empty #f (list set?/sc))
                (arr/sc (list any/sc) #f (list (listof/sc set?/sc))))))

    ;; more Or case
    (check-optimize
      ;; do not flatten nested or/sc containing flat & chaperone contracts (must be all flat or all chaperone)
      (or/sc cons?/sc (or/sc cons?/sc (box/sc cons?/sc)) (box/sc cons?/sc))
      #:pos (or/sc cons?/sc (or/sc cons?/sc (box/sc cons?/sc)) (box/sc cons?/sc))
      #:neg (or/sc cons?/sc (or/sc cons?/sc (box/sc cons?/sc)) (box/sc cons?/sc)))
    (check-optimize
      ;; flatten multiple or/sc
      (or/sc cons?/sc (or/sc set?/sc syntax?/sc) (or/sc list?/sc identifier?/sc))
      #:pos any/sc
      #:neg (or/sc identifier?/sc list?/sc syntax?/sc set?/sc cons?/sc))
    (check-optimize
      ;; flatten deeply-nested or/sc
      (or/sc cons?/sc (or/sc set?/sc (or/sc syntax?/sc (or/sc list?/sc identifier?/sc))))
      #:pos any/sc
      #:neg (or/sc set?/sc identifier?/sc list?/sc syntax?/sc cons?/sc))
    (let ([rec-chaperone (recursive-sc (list #'x) (list cons?/sc) (box/sc (recursive-sc-use #'x)))])
      (check-optimize
        (or/sc cons?/sc rec-chaperone)
        #:pos (or/sc cons?/sc rec-chaperone)
        #:neg (or/sc cons?/sc rec-chaperone)))
    (let ([rec-flat (recursive-sc (list #'x) (list cons?/sc) (and/sc set?/sc (recursive-sc-use #'x)))])
      (check-optimize
        (or/sc (box/sc cons?/sc) rec-flat)
        #:pos (or/sc (box/sc cons?/sc) rec-flat)
        #:neg (or/sc (box/sc cons?/sc) rec-flat)))
    (check-optimize
      ;; (or (and/sc ...)) where the "or" has a non-flat "and" is all flat --- don't optimize
      ;; this is just to make sure `and/sc` isn't treated specially
      (or/sc (box/sc cons?/sc) (and/sc cons?/sc list?/sc))
      #:pos (or/sc (box/sc cons?/sc) (and/sc cons?/sc list?/sc))
      #:neg (or/sc (box/sc cons?/sc) (and/sc cons?/sc list?/sc)))
    (check-optimize
      ;; (or (and ...)) where both contain flat contracts --- could optimize, but would need to realize this and/c is flat
      (or/sc set?/sc (and/sc cons?/sc list?/sc))
      #:pos (or/sc set?/sc (and/sc cons?/sc list?/sc))
      #:neg (or/sc set?/sc (and/sc cons?/sc list?/sc)))
    ))

(define tests
  (test-suite "Static Contracts"
    syntax-tests
    optimizer-tests))
