#lang racket/base

(require "../utils/utils.rkt"
         (rep rep-utils))

(provide (except-out (all-defined-out) seen-mark))


;;************************************************************
;; Current Seen Continuation Mark for Subtyping
;;************************************************************
;;
;; Prevents infinite loops when subtyping calls outside
;; functions that may then call subtyping

;; Type references/indirections that have been seen so far while
;; subtyping, including the following: Mus, Names, Structs, and Apps
(define seen-mark (make-continuation-mark-key 'seen))
(define (seen)
  (continuation-mark-set-first #f seen-mark null))
(define (currently-subtyping?)
  (continuation-mark-set-first #f seen-mark))

(define-syntax-rule (with-updated-seen A . body)
  (with-continuation-mark seen-mark A (let () . body)))

(define-syntax-rule (remember t1 t2 A)
  (cons (cons t1 t2) A))

(define-syntax-rule (remember* t1s/t2s A)
  (append t1s/t2s A))

(define-syntax-rule (seen? t1 t2 seen-ts)
  (for/or ([p (in-list seen-ts)])
    (and (equal? (car p) t1)
         (equal? (cdr p) t2))))
