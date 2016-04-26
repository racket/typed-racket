#lang racket/base


(module a typed/racket
  (provide num-id)
  (: num-id (∀ (A) (-> (∩ Number A) (∩ Number A))))
  (define (num-id x) x))

(require 'a)

(let ()
  (num-id 42)
  (void))

(with-handlers ([exn:fail:contract?
                 (λ (ex) (void))])
  (num-id "42"))


