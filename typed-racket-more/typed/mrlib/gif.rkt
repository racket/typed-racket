#lang typed/racket

;; A typed wrapper for mrlib/gif

(require typed/racket/draw)

(require/typed/provide
 mrlib/gif
 [write-gif (-> (U (Instance Bitmap%)
                (-> (Instance Bitmap%)))
                Path-String
                Void)]
 [write-animated-gif (-> (Listof (U (Instance Bitmap%)
                                    (-> (Instance Bitmap%))))
                         Natural Path-String
                         [#:loop? Any]
                         [#:one-at-a-time? Any]
                         [#:last-frame-delay (Option Natural)]
                         Void)])
