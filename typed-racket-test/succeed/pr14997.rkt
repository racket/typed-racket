#lang typed/racket

;; Test for PR 14997

(let*-values ([((a : Integer)) 1]) a)
(let*-values ([((a : Integer)) 1] [((b : Integer)) a]) a)
(let*-values: ([((a : Integer)) 1]) a)
(let*-values ([(a) 1]) a)
(let*-values ([(a) 1] [(b) a]) a)
