#lang typed/racket

(define: x : (Immutable-HashTable String String) #hash())
(ann #hash() (Immutable-HashTable String String))
