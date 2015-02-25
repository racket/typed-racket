#lang typed/racket

;; A typed wrapper for mrlib/bitmap-label

(require typed/racket/draw)

(require/typed/provide
 mrlib/bitmap-label
 [make-bitmap-label
  (->* [String (U (Instance Bitmap%) Path-String)]
       [(Instance Font%)]
       (Instance Bitmap%))]
 [bitmap-label-maker
  (-> String (U (Instance Bitmap%) Path-String)
      (-> Any (Instance Bitmap%)))])
