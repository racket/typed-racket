#lang racket/base
(require (for-syntax racket/base))
(provide assert defined?)

(define-syntax (assert stx)
  (syntax-case stx ()
    [(assert v)
     #`(let ([val v])
         #,(syntax-property
            (syntax/loc stx
              (or val
                  (raise-arguments-error 'assert
                   (format "Assertion ~s failed" '(assert v))
                   "given" val)))
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
