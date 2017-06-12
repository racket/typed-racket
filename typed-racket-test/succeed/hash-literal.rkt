#lang typed/racket

(define: x : (HashTable String String) #hash())
(ann #hash() (HashTable String String))

(define: x2 : (Immutable-HashTable String String) #hash())
(ann #hash() (Immutable-HashTable String String))
