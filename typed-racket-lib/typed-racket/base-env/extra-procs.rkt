#lang racket/base
(require (for-syntax racket/base))
(provide assert defined?)

(define-syntax (assert stx)
  (syntax-case stx (not/p)
    [(assert v) #'(assert v (not/p not))]
    [(assert v (not/p (not/p p))) #'(assert v p)]
    [(assert v (not/p p))
     #`(let ([val v] [pred p])
         #,(syntax-property
            (syntax/loc stx
              (if (pred val)
                  (raise-arguments-error 'assert
                   (format "Assertion ~s failed" '(assert v (not/p p)))
                   "expected" `(not/p ,(or (object-name pred) pred))
                   "given" val)
                  val))
            'feature-profile:TR-dynamic-check #t))]
    [(assert v p)
     #`(let ([val v] [pred p])
         #,(syntax-property
            (quasisyntax/loc stx
              (if (pred val)
                  val
                  (raise-arguments-error 'assert
                   (format "Assertion ~s failed" '(assert v p))
                   "expected" (or (object-name pred) pred)
                   "given" val)))
            'feature-profile:TR-dynamic-check #t))]))

(define (defined? v) #t)
