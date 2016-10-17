#lang typed/racket/base

(require typed/racket/draw)

(require "../private/require-batch.rkt")

;;; The Ray Tracer API has not stable enough yet,
;;; hence all the commented #:material arguments.
;;; As an alternative, please consider working
;;; with images/private/deep-flomap.rkt.

(require/typed/provide
 images/icons/style
 [default-icon-height (Parameterof Nonnegative-Real)]
 [toolbar-icon-height (Parameterof Nonnegative-Real)]
 [default-icon-backing-scale (Parameterof Positive-Real)]
 [light-metal-icon-color (U String (Instance Color%))]
 [metal-icon-color (U String (Instance Color%))]
 [dark-metal-icon-color (U String (Instance Color%))]
 [syntax-icon-color (U String (Instance Color%))]
 [halt-icon-color (U String (Instance Color%))]
 [run-icon-color (U String (Instance Color%))]
 [bitmap-render-icon
  (->* ((Instance Bitmap%))
       (Nonnegative-Real #| Flomap-Material |#)
       (Instance Bitmap%))]
 [icon-color->outline-color
  (-> (U String (Instance Color%)) (Instance Color%))])

(require/typed/provide
 images/icons/file
 [floppy-disk-icon
  (-> [#:color (U String (Instance Color%))]
      [#:height Nonnegative-Real]
      [#:backing-scale Positive-Real]
      (Instance Bitmap%))])

(require/typed/provide
 images/icons/symbol
 [text-icon
  (->* (String)
       ((Instance Font%)
        #:trim? Boolean
        #:color (U String (Instance Color%))
        #:height Nonnegative-Real
        ;#:material Flomap-Material
        #:outline Nonnegative-Real
        #:backing-scale Nonnegative-Real)
       (Instance Bitmap%))]
 [x-icon
  (-> [#:color (U String (Instance Color%))]
      [#:height Nonnegative-Real]
      ;[#:material Flomap-Material]
      [#:thickness Nonnegative-Real]
      [#:backing-scale Positive-Real]
      (Instance Bitmap%))])

(require/typed/provide
 images/icons/misc
 [regular-polygon-icon
  (->* (Positive-Integer #:color (U String (Instance Color%)))
       (Real #:height Nonnegative-Real
             ;#:material Flomap-Material
             #:backing-scale Nonnegative-Real)
       (Instance Bitmap%))]
 [foot-icon
  (-> #:color (U String (Instance Color%))
      [#:height Nonnegative-Real]
      ;[#:material Flomap-Material]
      [#:backing-scale Positive-Real]
      (Instance Bitmap%))]
 [close-icon
  (-> [#:color (U String (Instance Color%))]
      [#:height Nonnegative-Real]
      ;[#:material Flomap-Material]
      [#:backing-scale Positive-Real]
      (Instance Bitmap%))]
 [lock-icon
  (->* ()
       (Boolean #:body-color (U String (Instance Color%))
                #:shackle-color (U String (Instance Color%))
                #:height Nonnegative-Real
                ;#:material Flomap-Material
                #:backing-scale Nonnegative-Real)
       (Instance Bitmap%))])

(require/typed/provide
 images/icons/stickman
 [standing-stickman-icon
  (-> [#:head-color (U String (Instance Color%))]
      [#:arm-color (U String (Instance Color%))]
      [#:body-color (U String (Instance Color%))]
      [#:height Nonnegative-Real]
      ;[#:material Flomap-Material]
      [#:backing-scale Positive-Real]
      (Instance Bitmap%))]
 [running-stickman-icon
  (-> Real
      [#:head-color (U String (Instance Color%))]
      [#:arm-color (U String (Instance Color%))]
      [#:body-color (U String (Instance Color%))]
      [#:height Nonnegative-Real]
      ;[#:material Flomap-Material]
      [#:backing-scale Positive-Real]
      (Instance Bitmap%))])

(require/typed/provide
 images/icons/tool
 [debugger-bomb-color (U String (Instance Color%))]
 [macro-stepper-hash-color (U String (Instance Color%))]
 [small-macro-stepper-hash-color (U String (Instance Color%))])

(require/typed/provide/batch
 images/icons/arrow
 [id: right-arrow-icon left-arrow-icon up-arrow-icon down-arrow-icon
      right-over-arrow-icon left-over-arrow-icon
      right-under-arrow-icon left-under-arrow-icon]
 (-> #:color (U String (Instance Color%))
     [#:height Nonnegative-Real]
     ;[#:material Flomap-Material]
     [#:backing-scale Positive-Real]
     (Instance Bitmap%)))

(require/typed/provide/batch
 images/icons/control
 [id: bar-icon play-icon back-icon fast-forward-icon rewind-icon
      stop-icon record-icon pause-icon step-icon step-back-icon
      continue-forward-icon continue-backward-icon search-forward-icon
      search-backward-icon]
 (-> #:color (U String (Instance Color%))
     [#:height Nonnegative-Real]
     ;[#:material Flomap-Material]
     [#:backing-scale Positive-Real]
     (Instance Bitmap%)))

(require/typed/provide/batch
 images/icons/file
 [save-icon load-icon small-save-icon small-load-icon]
 (-> [#:disk-color (U String (Instance Color%))]
     [#:arrow-color (U String (Instance Color%))]
     [#:height Nonnegative-Real]
     ;[#:material Flomap-Material]
     [#:backing-scale Positive-Real]
     (Instance Bitmap%)))

(require/typed/provide/batch
 images/icons/symbol
 [check-icon recycle-icon lambda-icon hash-quote-icon]
 (-> [#:color (U String (Instance Color%))]
     [#:height Nonnegative-Real]
     ;[#:material Flomap-Material]
     [#:backing-scale Positive-Real]
     (Instance Bitmap%)))

(require/typed/provide/batch
 images/icons/misc
 [stop-sign-icon stop-signs-icon]
 (-> [#:color (U String (Instance Color%))]
     [#:height Nonnegative-Real]
     ;[#:material Flomap-Material]
     [#:backing-scale Positive-Real]
     (Instance Bitmap%)))

(require/typed/provide/batch
 images/icons/misc
 [clock-icon stopwatch-icon]
 (->* ()
      (Byte Byte
            #:face-color (U String (Instance Color%))
            #:hand-color (U String (Instance Color%))
            #:height Nonnegative-Real
            #:backing-scale Nonnegative-Real)
      (Instance Bitmap%)))

(require/typed/provide/batch
 images/icons/misc
 [stethoscope-icon short-stethoscope-icon]
 (-> [#:color (U String (Instance Color%))]
     [#:height Nonnegative-Real]
     [#:backing-scale Positive-Real]
     (Instance Bitmap%)))

(require/typed/provide/batch
 images/icons/misc
 [bomb-icon left-bomb-icon]
 (-> [#:cap-color (U String (Instance Color%))]
     [#:bomb-color (U String (Instance Color%))]
     [#:height Nonnegative-Real]
     ;[#:material Flomap-Material]
     [#:backing-scale Positive-Real]
     (Instance Bitmap%)))

(require/typed/provide/batch
 images/icons/misc
 [magnifying-glass-icon left-magnifying-glass-icon]
 (-> [#:frame-color (U String (Instance Color%))]
     [#:handle-color (U String (Instance Color%))]
     [#:height Nonnegative-Real]
     ;[#:material Flomap-Material]
     [#:backing-scale Positive-Real]
     (Instance Bitmap%)))

(require/typed/provide/batch
 images/icons/tool
 [id: check-syntax-icon small-check-syntax-icon macro-stepper-icon
      small-macro-stepper-icon debugger-icon small-debugger-icon]
 (-> [#:height Nonnegative-Real]
     ;[#:material Flomap-Material]
     [#:backing-scale Positive-Real]
     (Instance Bitmap%)))
