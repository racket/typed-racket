#lang racket/base
(require (for-syntax racket/base))
(provide assert defined?)

(define-syntax (assert stx)
  (syntax-case stx ()
    [(assert v)
     #`(let ([val v])
         #,(syntax-property
            (syntax/loc stx
              (or val (error (format "Assertion failed on ~e" val))))
            'feature-profile:TR-dynamic-check #t))]
    [(assert v p)
     #`(let ([val  v]
             [pred p])
         #,(syntax-property
            (quasisyntax/loc stx
              (if (pred val)
                  val
                  (error (format "Assertion ~e failed on ~e" pred val))))
            'feature-profile:TR-dynamic-check #t))]))

(define (defined? v) #t)
