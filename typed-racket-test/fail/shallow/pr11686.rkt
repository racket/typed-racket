#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang racket/load

(module T typed/racket/shallow

  (struct: [X] doll ([contents : X]))

  (define-type RussianDoll
    (Rec RD (U 'center (doll RD))))

  (: f (RussianDoll -> RussianDoll))
  (define (f rd)
    (let ((v (doll-contents (assert rd doll?))))
      (assert v doll?)))

  (: md (All (x) (x -> (doll x))))
  (define md doll)

  (provide (all-defined-out)))

(module U racket
 (require 'T)
 (f (md 3)))

(require 'U)




