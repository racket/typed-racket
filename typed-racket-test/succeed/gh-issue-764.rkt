#lang typed/racket

(ann (= 0 +nan.0) False)
(ann (= +nan.0 0) False)

(ann (= 0 +0.0) True)
(ann (= +0.0 0) True)

(ann (= 0 -0.0) True)
(ann (= -0.0 0) True)

(ann (= -0.0 +0.0) True)

; This was always ok:
(ann (= 1 +nan.0) False)

(ann (= 0 1) False)
(ann (= 1 0) False)

