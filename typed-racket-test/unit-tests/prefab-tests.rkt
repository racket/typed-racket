#lang racket/base

;; Unit tests for prefab type helpres

(require "test-utils.rkt"
         racket/list
         racket/contract
         rackunit
         typed-racket/utils/prefab
         typed-racket/utils/prefab-c)

(provide tests)
(gen-test-main)

(define-check (check-normalize key n norm)
  (check-equal? (normalize-prefab-key key n) norm))

;; check that the abbreviate function is consistent with how
;; Racket abbreviates in the run-time
(define-check (check-abbreviate key n)
  (check-equal? (abbreviate-prefab-key key)
                (prefab-struct-key
                  (apply make-prefab-struct key (make-list n 0)))))

(struct pair (fst snd) #:prefab)
(define raw-p (pair 1 2))
(define p0
  (contract
   (prefab/c 'pair integer? integer?)
   raw-p
   'pos
   'neg))
(define p1
  (contract
   (prefab/c 'pair integer? (vectorof integer?))
   (pair 1 (vector 2))
   'pos
   'neg))
(define p2
  (contract
   (prefab/c 'pair integer? (vectorof integer?))
   `#s(pair 1 ,(vector 2))
   'pos
   'neg))
(struct mpair (fst snd) #:prefab #:mutable)
(define mpair/c (prefab/c '(mpair #(0 1)) integer? (vectorof integer?)))
(define mp1 (contract mpair/c (mpair 1 (vector 2)) 'pos 'neg))
(define p3
  (contract
   (prefab/c 'pair integer? (vectorof integer?))
   (pair 1 (vector "1"))
   'pos
   'neg))

(define p4
  (contract (struct/c pair integer? (vectorof integer?))
            (pair 1 (vector "1"))
            'pos
            'neg))

(define tests
  (test-suite
   "Tests for prefab type helpers"
   (check-normalize '(foo) 1 '(foo 1 (0 #f) #()))
   (check-normalize '(foo) 3 '(foo 3 (0 #f) #()))
   (check-normalize '(foo 1) 1 '(foo 1 (0 #f) #()))
   (check-normalize '(foo 3) 3 '(foo 3 (0 #f) #()))
   (check-normalize '(foo bar 3) 4 '(foo 1 (0 #f) #() bar 3 (0 #f) #()))
   (check-normalize '(foo 3 bar 3) 6 '(foo 3 (0 #f) #() bar 3 (0 #f) #()))
   (check-normalize '(foo 1 #()) 1 '(foo 1 (0 #f) #()))
   (check-normalize '(foo 1 #(0)) 1 '(foo 1 (0 #f) #(0)))
   (check-normalize '(foo 5 (0 #f)) 5 '(foo 5 (0 #f) #()))
   (check-normalize '(foo 5 (1 #f)) 6 '(foo 5 (1 #f) #()))
   (check-normalize '(foo 5 (0 #f) #()) 5 '(foo 5 (0 #f) #()))
   (check-normalize '(foo bar 4) 7 '(foo 3 (0 #f) #() bar 4 (0 #f) #()))
   (check-normalize '(foo (1 #f) bar 4) 8 '(foo 3 (1 #f) #() bar 4 (0 #f) #()))
   (check-normalize '(foo #(1) bar 4) 7 '(foo 3 (0 #f) #(1) bar 4 (0 #f) #()))
   (check-normalize '(foo bar 4 (1 #f)) 8 '(foo 3 (0 #f) #() bar 4 (1 #f) #()))
   (check-normalize '(foo bar 1 baz 4 (1 #f))
                    8
                    '(foo 2 (0 #f) #() bar 1 (0 #f) #() baz 4 (1 #f) #()))

   (check-equal? (prefab-key->field-count '(foo 1 (0 #f) #()))
                 1)
   (check-equal? (prefab-key->field-count '(foo 1 (1 #f) #()))
                 2)
   (check-equal? (prefab-key->field-count
                  '(foo 1 (1 #f) #() bar 1 (0 #f) #()))
                 3)
   (check-equal? (prefab-key->field-count
                  '(foo 1 (1 #f) #() bar 1 (0 #f) #() baz 2 (0 #f) #()))
                 5)

   (check-abbreviate '(foo) 1)
   (check-abbreviate '(foo 1) 1)
   (check-abbreviate '(foo 3) 3)
   (check-abbreviate '(foo bar 3) 4)
   (check-abbreviate '(foo 3 bar 3) 6)
   (check-abbreviate '(foo 1 #()) 1)
   (check-abbreviate '(foo 1 #(0)) 1)
   (check-abbreviate '(foo 5 (0 #f)) 5)
   (check-abbreviate '(foo 5 (1 #f)) 6)
   (check-abbreviate '(foo 5 (0 #f) #()) 5)
   (check-abbreviate '(foo 5 (1 #f) #()) 6)
   (check-abbreviate '(foo 5 (0 #f) #(1)) 5)
   (check-abbreviate '(foo 5 (1 #f) #(1)) 6)
   (check-abbreviate '(foo 5 (0 #f) #() bar 1) 6)
   (check-abbreviate '(foo 5 (0 #f) #(1) bar 1) 6)
   (check-abbreviate '(foo 5 (1 #f) #(1) bar 1) 7)
   (check-abbreviate '(foo 5 (1 #f) #(1) bar 1 #()) 7)
   (check-abbreviate '(foo 5 (0 #f) #(1) bar 1 #()) 6)
   (check-abbreviate '(foo 5 (0 #f) #() bar 1 #()) 6)
   (check-abbreviate '(foo 5 (1 #f) #(1) bar 1 #(0)) 7)

   (check-true (prefab-key-subtype? '(foo 1 (0 #f) #() bar 1 (0 #f) #())
                                    '(bar 1 (0 #f) #())))
   (check-false (prefab-key-subtype? '(foo 1 (0 #f) #())
                                     '(bar 1 (0 #f) #())))

   (check-equal? (prefab-key->field-mutability
                  '(foo 1 (0 #f) #() bar 1 (0 #f) #()))
                 (list #f #f))
   (check-equal? (prefab-key->field-mutability
                  '(foo 2 (0 #f) #(0) bar 3 (0 #f) #(1)))
                 (list #f #t #f #t #f))
   (check-equal? (prefab-key->field-mutability
                  '(foo 2 (0 #f) #(0) bar 3 (0 #f) #(1) baz 1 (0 #f) #(0)))
                 (list #t #f #t #f #t #f))
   ;; immutable prefab example
   (check-eq? raw-p p0)
   (check-equal? (pair-fst p0) 1)
   (check-equal? (pair-snd p0) 2)
   (check-exn (λ (e) (and (exn:fail:contract:blame? e)
                          (eq? 'pos (blame-positive (exn:fail:contract:blame-object e)))
                          (eq? 'neg (blame-negative (exn:fail:contract:blame-object e)))))
              (λ () (contract
                     (prefab/c 'pair integer? integer?)
                     (pair 1 "2")
                     'pos
                     'neg)))
   (check-exn (λ (e) (and (exn:fail:contract:blame? e)
                          (eq? 'pos (blame-positive (exn:fail:contract:blame-object e)))
                          (eq? 'neg (blame-negative (exn:fail:contract:blame-object e)))))
              (λ () (contract
                     (prefab/c 'pair integer? integer?)
                     (pair "1" 2)
                     'pos
                     'neg)))
   (check-exn (λ (e) (and (exn:fail:contract:blame? e)
                          (eq? 'pos (blame-positive (exn:fail:contract:blame-object e)))
                          (eq? 'neg (blame-negative (exn:fail:contract:blame-object e)))))
              (λ () (contract
                     (prefab/c 'pair integer? (vectorof integer?))
                     (pair 1 1)
                     'pos
                     'neg)))
   ;; this error shouldn't be reported at creation
   (check-not-exn
    (λ () (contract
           (prefab/c 'pair integer? (vectorof integer?))
           (pair 1 (vector "2"))
           'pos
           'neg)))
   ;; error when prefab key and struct are a mismatch
   (check-exn exn:fail:contract?
              (λ () (contract
                     (prefab/c '(pair #(0 1)) integer? integer?)
                     (pair 1 2)
                     'pos
                     'neg)))
  ;; accessing fields
   (check-equal? (pair-fst p1) 1)
   (check-equal? (pair-fst p2) 1)
   (check-equal? (pair-snd p1) (vector 2))
   (check-equal? (pair-snd p2) (vector 2))
   ;; higher order contract on field value
   (check-exn (λ (e) (and (exn:fail:contract:blame? e)
                          (eq? 'neg (blame-positive (exn:fail:contract:blame-object e)))
                          (eq? 'pos (blame-negative (exn:fail:contract:blame-object e)))))
              (λ () (vector-set! (pair-snd p1)
                                 0
                                 "1")))
   (check-exn (λ (e) (and (exn:fail:contract:blame? e)
                          (eq? 'neg (blame-positive (exn:fail:contract:blame-object e)))
                          (eq? 'pos (blame-negative (exn:fail:contract:blame-object e)))))
              (λ () (vector-set! (pair-snd p2)
                                 0
                                 "1")))
   (check-not-exn (λ () (vector-set! (pair-snd p1)
                                     0
                                     42)))
   (check-not-exn (λ () (vector-set! (pair-snd p2)
                                     0
                                     42)))
   (check-equal? (vector-ref (pair-snd p1) 0) 42)
   (check-equal? (vector-ref (pair-snd p2) 0) 42)

   ;; mutable prefab example
   ;; check contracting bad struct fails
   (check-exn (λ (e) (and (exn:fail:contract:blame? e)
                          (eq? 'pos (blame-positive (exn:fail:contract:blame-object e)))
                          (eq? 'neg (blame-negative (exn:fail:contract:blame-object e)))))
              (λ () (contract mpair/c (mpair "1" (vector 2)) 'pos 'neg)))
   (check-exn (λ (e) (and (exn:fail:contract:blame? e)
                          (eq? 'pos (blame-positive (exn:fail:contract:blame-object e)))
                          (eq? 'neg (blame-negative (exn:fail:contract:blame-object e)))))
              (λ () (contract mpair/c (mpair "1" (vector "2")) 'pos 'neg)))

   ;; accessing fields
   (check-equal? (mpair-fst mp1) 1)
   (check-equal? (mpair-snd mp1) (vector 2))
   ;; violating higher order contract on field value
   (check-exn (λ (e) (and (exn:fail:contract:blame? e)
                          (eq? 'neg (blame-positive (exn:fail:contract:blame-object e)))
                          (eq? 'pos (blame-negative (exn:fail:contract:blame-object e)))))
              (λ () (set-mpair-fst! mp1 "0")))
   (check-exn (λ (e) (and (exn:fail:contract:blame? e)
                          (eq? 'neg (blame-positive (exn:fail:contract:blame-object e)))
                          (eq? 'pos (blame-negative (exn:fail:contract:blame-object e)))))
              (λ () (vector-set! (mpair-snd mp1)
                                 0
                                 "1")))
   (check-not-exn (λ () (vector-set! (mpair-snd mp1)
                                     0
                                     42)))
   (check-equal? (vector-ref (pair-snd p2) 0) 42)
   (check-not-exn (λ () (set-mpair-fst! mp1 0)))
   (check-not-exn (λ () (set-mpair-snd! mp1 (vector 33))))
   (check-equal? (mpair-fst mp1) 0)
   (check-equal? (mpair-snd mp1) (vector 33))


   ;; contract stronger tests
   (check-true (contract-stronger? (prefab/c 'pair integer? integer?) any/c))
   (check-false (contract-stronger? (prefab/c 'pair integer? (vectorof integer?)) integer?))
   
   ;; key/field count checks
   (check-true (contract-stronger? (prefab/c 'pair integer? (vectorof integer?))
                                   (prefab/c 'pair integer? (vectorof integer?))))
   (check-true (contract-stronger? (prefab/c '(mpair #(0 1)) integer? (vectorof integer?))
                                   (prefab/c '(mpair #(0 1)) integer? (vectorof integer?))))
   (check-false (contract-stronger? (prefab/c 'pair integer? (vectorof integer?))
                                    (prefab/c 'posn integer? (vectorof integer?))))
   (check-false (contract-stronger? (prefab/c 'pair integer? (vectorof integer?))
                                    (prefab/c '(pair #(0 1)) integer? (vectorof integer?))))
   (check-false (contract-stronger? (prefab/c 'pair integer? (vectorof integer?))
                                    (prefab/c 'pair integer? (vectorof integer?) (vectorof integer?))))
   (check-false (contract-stronger? (prefab/c 'pair integer? (vectorof integer?) (vectorof integer?))
                                    (prefab/c 'pair integer? (vectorof integer?))))

   ;; field stronger checks
   (check-true (contract-stronger? (prefab/c 'pair integer? integer?)
                                   (prefab/c 'pair (or/c integer? symbol?) integer?)))
   (check-false (contract-stronger? (prefab/c '(pair #(0 1)) integer? integer?)
                                   (prefab/c '(pair #(0 1)) (or/c integer? symbol?) integer?)))
   (check-true (contract-stronger? (prefab/c 'pair integer? integer?)
                                   (prefab/c 'pair integer? (or/c integer? symbol?))))
   (check-false (contract-stronger? (prefab/c '(pair #(0 1)) integer? integer?)
                                    (prefab/c 'pair integer? (or/c integer? symbol?))))
   (check-true (contract-stronger? (prefab/c 'pair integer? integer?)
                                   (prefab/c 'pair (or/c integer? symbol?) (or/c integer? symbol?))))
   (check-false (contract-stronger? (prefab/c 'pair (or/c integer? symbol?) integer?)
                                    (prefab/c 'pair integer? integer?)))
   (check-false (contract-stronger? (prefab/c 'pair integer? (or/c integer? symbol?))
                                    (prefab/c 'pair integer? integer?)))
   (check-false (contract-stronger? (prefab/c 'pair (or/c integer? symbol?) (or/c integer? symbol?))
                                    (prefab/c 'pair integer? integer?)))

   ;; sub-key checks
   ;; These ideally should work, but there isn't a general purpose "prefab sub-key" procedure
   ;; we can use at the moment.
;   (check-true (contract-stronger? (prefab/c '(3dposn posn 2) integer? integer? integer?)
;                                   (prefab/c 'posn integer? integer?)))
;   (check-true (contract-stronger? (prefab/c '(3dposn #(0) posn 2) integer? integer? integer?)
;                                   (prefab/c 'posn integer? integer?)))
;   (check-true (contract-stronger? (prefab/c '(3dposn #(0) posn 2) integer? integer? integer?)
;                                   (prefab/c 'posn (or/c integer? symbol?) integer?)))
;   (check-true (contract-stronger? (prefab/c '(3dposn #(0) posn 2) integer? integer? integer?)
;                                   (prefab/c 'posn integer? (or/c integer? symbol?))))
;   (check-false (contract-stronger? (prefab/c 'posn integer? integer?)
;                                    (prefab/c '(3dposn posn 2) integer? integer? integer?)))
;   (check-true (contract-stronger? (prefab/c '(3dposn posn 2) integer? integer? integer?)
;                                   (prefab/c 'posn (or/c integer? symbol?) integer?)))
;   (check-true (contract-stronger? (prefab/c '(3dposn posn 2) integer? integer? integer?)
;                                   (prefab/c 'posn integer? (or/c integer? symbol?))))
;   (check-false (contract-stronger? (prefab/c '(3dposn posn 2) (or/c integer? symbol?) integer? integer?)
;                                    (prefab/c 'posn integer? integer?)))
;   (check-false (contract-stronger? (prefab/c '(3dposn posn 2) integer? (or/c integer? symbol?) integer?)
;                                    (prefab/c 'posn integer? integer?)))
;   (check-true (contract-stronger? (prefab/c '(3dposn posn 2) integer? integer? (or/c integer? symbol?))
;                                   (prefab/c 'posn integer? integer?)))

   ;; contract equivalent checks
   (check-false (contract-stronger? (prefab/c 'pair integer? (vectorof integer?)) any/c))
   (check-false (contract-stronger? (prefab/c 'pair integer? (vectorof integer?)) integer?))
   (check-true (contract-equivalent? (prefab/c 'pair integer? (vectorof integer?))
                                     (prefab/c 'pair integer? (vectorof integer?))))
   (check-false (contract-equivalent? (prefab/c 'pair integer? (vectorof integer?))
                                      (prefab/c 'posn integer? (vectorof integer?))))
   (check-false (contract-equivalent? (prefab/c 'pair integer? (vectorof integer?))
                                      (prefab/c 'pair (or/c integer? symbol?) (vectorof integer?))))
   (check-false (contract-equivalent? (prefab/c 'pair (or/c integer? symbol?) (vectorof integer?))
                                      (prefab/c 'pair integer? (vectorof integer?))))
   (check-true (contract-equivalent? (prefab/c 'pair (or/c integer? symbol?) (vectorof integer?))
                                     (prefab/c 'pair (or/c symbol? integer?) (vectorof integer?))))
   (check-true (contract-equivalent? (prefab/c '(mpair #(0 1)) (or/c integer? symbol?) (vectorof integer?))
                                     (prefab/c '(mpair #(0 1)) (or/c symbol? integer?) (vectorof integer?))))
   (check-false (contract-equivalent? (prefab/c 'posn integer? integer?)
                                      (prefab/c '(3dposn posn 2) integer? integer? integer?)))
   (check-false (contract-equivalent? (prefab/c '(3dposn posn 2) integer? integer? integer?)
                                      (prefab/c 'posn integer? integer?)))
   (check-false (contract-equivalent? (prefab/c 'posn integer? integer?)
                                      (prefab/c '(3dposn posn 2) integer? integer? integer?)))
   ))
