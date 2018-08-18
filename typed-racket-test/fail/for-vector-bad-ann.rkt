#;
(exn-pred 3)
#lang typed/racket

(for/vector : (Immutable-Vectorof Any) ([x (in-list '())]) x)
