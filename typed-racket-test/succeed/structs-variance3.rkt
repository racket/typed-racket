#lang typed/racket

(struct (A) Node
         ([elem  : A]
          [left  : (Tree A)]
          [right : (Tree A)]
          [prio  : Real]))

(define-type (Tree A) (U Null (Node A)))

;; (define-type (Func A) (A A -> Boolean))

;; (struct (A) Treap
;;          ([comparer : (Func A)]
;;           [tree     : (Tree A)]
;;           [size     : Integer]))

(ann (ann null (Tree Natural)) (Tree Number))
;; (ann (ann null (Tree)))
