#lang typed/racket/optional

(require
  plot/no-gui
  typed/racket/draw
  typed/rackunit)

(check-not-exn
  (lambda () (void (plot-pict (function values -10 10)))))

(define bad-renderers (cast (list 42) (Listof renderer2d)))
(check-exn exn:fail:contract?
  (lambda () (plot-pict bad-renderers)))

(define bad-point (cast (vector 'X 'Y) (Vector Real Real)))
(check-exn exn:fail:contract?
  (lambda () (points (vector bad-point))))

;; check for no type errors
(void
  plot/dc plot-bitmap plot-pict plot3d/dc)
