#;
(exn-pred "match:")
#lang typed/racket
(ann (match '(b) [(list 'a) 42]) Number)
