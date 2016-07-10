#lang typed/racket

;; Reported by Matthew Eric Basset
;; https://groups.google.com/forum/#!searchin/racket-users/weirdness%7Csort:date/racket-users/g6UmwgZWtzE/1czSRfk2AgAJ

;; Works "as expected" on 6.5
(ann (hash 'a 1 'b "cat") (HashTable Any Any))

;; Does not work on 6.5
(define t* (hash 'a 1 'b "cat"))
(ann t* (HashTable Any Any))

;; Also does not work on 6.5
(ann t* (HashTable Symbol (U String Integer)))
