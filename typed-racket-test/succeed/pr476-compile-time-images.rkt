#lang typed/racket

(require typed/images/compile-time)

(require (for-syntax images/icons/stickman))

(cons (compiled-bitmap (standing-stickman-icon #:height 8 #:head-color "red" #:body-color "red") 90)
      (compiled-bitmap-list (for/list ([step (in-range 0.0 1.0 1/12)]) (running-stickman-icon step #:height 8))))
