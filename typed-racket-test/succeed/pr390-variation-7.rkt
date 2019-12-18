#lang typed/racket/base
(require (for-syntax racket/base))

(define-syntax (mhash stx)
  #`#'#,(make-hash '((a . a))))

(define-syntax (ihash stx)
  #`#'#,(make-immutable-hash))

(define-syntax (whash stx)
  #`#'#,(make-weak-hash '((a . a))))

(void
  (ann (mhash) (Syntaxof (Mutable-HashTable Symbol Symbol)))
  (ann (ihash) (Syntaxof (Immutable-HashTable Symbol Symbol)))
  (ann (whash) (Syntaxof (Weak-HashTable Symbol Symbol))))
