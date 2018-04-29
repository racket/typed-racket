#lang typed/racket/base/optional

;; Test:
;; 1. enclosing module is locally-defensive
;; 2. contains a Racket submodule
;; 3. import a value with the right constructor & the wrong type
;; 4. send the value back to untyped code, get low-level error from untyped

(require "module-server.rkt" ) ;; import a typed function

(: h (-> Real Real))
(define (h n)
  (* n n))

(f 3 h)


(module u racket/base
  (provide xs)
  (define xs '(A B C)))
(require/typed 'u
  (xs (Listof Natural)))

(with-handlers ((exn:fail:contract? (lambda (_) #true)))
  (filter zero? xs))
