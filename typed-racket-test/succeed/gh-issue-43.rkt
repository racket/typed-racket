#lang typed/racket

(let loop : Integer ([n 10])
  (cond
    [(= n 5) n]
    [else (loop (sub1 n))]))

(let loop : Integer ([n : Integer 10])
  (cond
    [(= n 5) n]
    [else (loop (sub1 n))]))
