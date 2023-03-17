#lang typed/racket/base

(require racket/fixnum
         racket/flonum)

(ann (< 42) True)
(ann (> 42) True)
(ann (<= 42) True)
(ann (>= 42) True)
(ann (fx< 42) True)
(ann (fx> 42) True)
(ann (fx<= 42) True)
(ann (fx>= 42) True)
(ann (fl< 42.0) True)
(ann (fl> 42.0) True)
(ann (fl<= 42.0) True)
(ann (fl>= 42.0) True)
