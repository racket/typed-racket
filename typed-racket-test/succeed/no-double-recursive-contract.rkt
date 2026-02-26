#lang racket/base

;; Test that recursive Name types produce well-formed contracts.
;; Exercises Name->Mu, Name->Name->Mu, mutually recursive names,
;; and plain Mu types.

(require rackunit
         racket/class)

;; ---------------------------------------------------------------
;; Case 1: Name type resolving to Mu (recursive class type alias)
(module typed1 typed/racket
  (require typed/racket/class)
  (define-type C% (Class [m (-> (Instance C%) Void)]))
  (: obj (Instance C%))
  (define obj
    (new (class object%
           (super-new)
           (define/public (m x) (void)))))
  (provide obj))

(require 'typed1)
(check-true (object? obj))
(send obj m obj)

;; ---------------------------------------------------------------
;; Case 2: Name type alias chain (Name->Name->Mu)
(module typed2 typed/racket
  (require typed/racket/class)
  (define-type Base% (Class [m (-> (Instance Base%) Void)]))
  (define-type Alias% Base%)
  (: obj2 (Instance Alias%))
  (define obj2
    (new (class object%
           (super-new)
           (define/public (m x) (void)))))
  (provide obj2))

(require 'typed2)
(check-true (object? obj2))
(send obj2 m obj2)

;; ---------------------------------------------------------------
;; Case 3: Mutually recursive Name types
(module typed3 typed/racket
  (require typed/racket/class)
  (define-type A% (Class [get-b (-> (Instance B%))]))
  (define-type B% (Class [get-a (-> (Instance A%))]))
  (: a-obj (Instance A%))
  (: b-obj (Instance B%))
  (define b-obj
    (new (class object%
           (super-new)
           (define/public (get-a) a-obj))))
  (define a-obj
    (new (class object%
           (super-new)
           (define/public (get-b) b-obj))))
  (provide a-obj
           b-obj))

(require 'typed3)
(check-true (object? a-obj))
(check-true (object? b-obj))
(check-true (object? (send a-obj get-b)))
(check-true (object? (send b-obj get-a)))

;; ---------------------------------------------------------------
;; Case 4: Recursive non-class Mu type (e.g., recursive list)
;; to make sure the fix doesn't break plain Mu types
(module typed4 typed/racket
  (define-type Tree (U Null (Pairof Integer Tree)))
  (: t Tree)
  (define t (list 1 2 3))
  (provide t))

(require 'typed4)
(check-equal? t '(1 2 3))
