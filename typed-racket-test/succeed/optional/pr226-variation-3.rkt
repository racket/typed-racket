#lang typed/racket/optional

(module untyped racket
  (struct s ())
  (define (s?? x)
    (when (box? x)
      (set-box! x (void)))
    #t)
  (provide s struct:s (rename-out [s?? s?])))

(require/typed 'untyped
  [#:struct s ()])

(: suitcase (Boxof '$$$))
(define suitcase (box '$$$))

(with-handlers ([exn:fail:contract? (lambda (x) (void))])
  (s? suitcase)
  (void))

(require typed/rackunit)
(check-not-exn
  (lambda () (unbox suitcase)))
