#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang racket/load

(module typed typed/racket/shallow
  (provide g)

  (define-type Foo (Rec a (U (List Any) (Boxof a))))


  (: f (Byte -> Natural))
  (define (f x) (add1 x))
  (: g (Foo -> Void))
  (define (g b) 
    (when (box? b)
      (set-box! b (list f)))))

(require 'typed)
(define b (box (list #f)))
(g b)
(displayln ((first (unbox b)) "foo"))
