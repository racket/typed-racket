#lang typed/racket

(define-type In-Integer (-> (Listof (List Integer Number Float)) (Sequenceof Integer Number Float)))

(: test-int (-> In-Integer (Listof (List Integer Number Float)) Number))
(define (test-int in-ints ints)
  (for/sum : Number ([([a : Integer] [b : Number] [c : Float]) (in-ints ints)])
    (+ a b c)))

(for/list : (Listof Integer) ([i : Integer (list 0 1)])
  (+ i 1))

(for/list : (Listof Integer) ([i : Integer (in-list (list 0 1))])
  (+ i 1))
