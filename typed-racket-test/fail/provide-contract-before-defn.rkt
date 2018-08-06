#;
(exn-pred #rx"f-ctc: undefined")

#lang typed/racket
(provide/contract
 [f f-ctc])

(: f (-> Integer Integer))
(define (f n)
  (+ 1 n))

;; annotate so that the error message is about f-ctc being undefined and not
;; about a missing type for the identifier
(: f-ctc (Contract (-> Integer Any) (-> Any Integer)))
(define f-ctc (->/c exact-integer? exact-integer?))
