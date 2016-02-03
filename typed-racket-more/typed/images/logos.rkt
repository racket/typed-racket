#lang typed/racket

(require typed/racket/draw)
(require "../private/require-batch.rkt")

(require/typed/provide/batch
 images/logos
 [plt-logo planet-logo stepper-logo macro-stepper-logo]
 (-> [#:height Nonnegative-Real]
     [#:backing-scale Nonnegative-Real]
     (Instance Bitmap%)))
