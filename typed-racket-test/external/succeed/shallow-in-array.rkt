#lang typed/racket/shallow

;; Use 'in-array'
;; inspired by jpeg benchmark


(require (only-in math/array in-array Array))

(: read-dct-scan (-> (Array Integer) Void))
(define (read-dct-scan dest)
  (for ((mcu (in-array dest)))
    (void)))
