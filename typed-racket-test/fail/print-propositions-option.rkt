#;
(exn-pred #rx"\\(-> Any Boolean : #:\\+ \\(: x String\\) #:- Top\\)")
#lang typed/racket #:print-propositions

;; With #:print-propositions, types in error messages include their
;; latent propositions, which are otherwise omitted here.

(define x : Any 1)
(define f : (U Integer (-> Any Boolean : #:+ (: x String))) 1)
(f 1)
