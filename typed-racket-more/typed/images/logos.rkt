#lang typed/racket/base

(require typed/racket/draw)
(require "../private/require-batch.rkt")

(require/typed/provide/batch
 images/logos
 [plt-logo planet-logo stepper-logo macro-stepper-logo]
 (-> [#:height Nonnegative-Real]
     [#:backing-scale Positive-Real]
     (Instance Bitmap%)))
