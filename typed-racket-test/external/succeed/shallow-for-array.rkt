#lang typed/racket/shallow

;; Use 'for/array' and `array-shape`
;; inspired by jpeg benchmark
;;
;; Must use both!

(require
  (only-in math/array for/array: for/array in-array array-shape Array))

(: bg (-> (Array Symbol) Void))
(define (bg mcu-array)
  (void
    ;; well this works ...
    (for/array #:shape (vector-map (ann values (-> Index Integer)) (array-shape mcu-array))
               ((mcu : Symbol (in-array mcu-array)))
      (void))))

