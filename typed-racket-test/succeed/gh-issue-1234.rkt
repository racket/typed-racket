#lang racket/base

(require racket/sandbox)

(call-with-limits
 20
 500
 (λ () (eval '(begin (module a typed/racket
                       (provide (all-defined-out))

                       (define-type (Formula a)
                         (U
                          True
                          False
                          (Atom a)
                          (Not a)
                          (And a)
                          (Or a)
                          (Imp a)
                          (Iff a)
                          (Forall a)
                          (Exists a)
                          ))

                       (struct False () #:transparent)
                       (struct True () #:transparent)
                       (struct (a) Atom ([f : a]) #:transparent)
                       (struct (a) Not ([f : (Formula a)]) #:transparent)
                       (struct (a) And ([l : (Formula a)] [r : (Formula a)]) #:transparent)
                       (struct (a) Or ([l : (Formula a)] [r : (Formula a)]) #:transparent)
                       (struct (a) Imp ([pre : (Formula a)] [post : (Formula a)]) #:transparent)
                       (struct (a) Iff ([l : (Formula a)] [r : (Formula a)]) #:transparent)
                       (struct (a) Forall ([v : String] [f : (Formula a)]) #:transparent)
                       (struct (a) Exists ([v : String] [f : (Formula a)]) #:transparent))
                     (require 'a))
             (make-base-namespace))))
