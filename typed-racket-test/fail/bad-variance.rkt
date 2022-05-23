#;
(exn-pred 2)
#lang typed/racket/base

(struct (A) Node
         ([elem  : A]
          [left  : (Tree A)]
          [right : (Tree A)]
          [prio  : Real]))

(define-type (Tree A) (U Null (Node A)))

(define-type (Func A) (A A -> Boolean))

(struct (A) Treap
         ([comparer : (Func A)]
          [tree     : (Tree A)]
          [size     : Integer]))

(lambda ([a : (Treap Integer)]) : Void
  (ann a (Treap Real))
  (ann a (Treap Natural))
  (void))
