#lang typed/racket/base

(require typed/racket/class typed/racket/draw)

(: get-cached-font (-> String Font-Weight Font-Style (Instance Font%)))
(define (get-cached-font font weight style)
  (make-font #:size 1024.0 #:style style #:weight weight #:face font))

