#lang racket

(require rackunit
         typed-racket/typed-reader)

(test-case
 "Annotation reader syntax"
 (define stx (read-syntax 'in (open-input-string "#{x : T}")))
 (check-equal? (syntax-e stx) 'x)
 (check-equal? (syntax-property stx 'type-label) 'T))

(test-case
 "Overridden annotation reader syntax"
 (define (read* c in . args)
   (read-char in)
   'replacement)
 (define stx
   (parameterize ([current-readtable
                   (make-readtable #f #\{ 'dispatch-macro read*)])
     (read-syntax 'in (open-input-string "#{}"))))
 (check-equal? (syntax-e stx) 'replacement))
