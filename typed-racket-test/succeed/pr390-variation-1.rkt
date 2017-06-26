#lang typed/racket

;; Reported by Matthew Eric Basset
;; https://groups.google.com/forum/#!searchin/racket-users/weirdness%7Csort:date/racket-users/g6UmwgZWtzE/1czSRfk2AgAJ

;; Works "as expected" on 6.5
(void
  (ann (hash 'a 1 'b "cat") (HashTable Any Any)))

(define t* (hash 'a 1 'b "cat"))

;; These `ann` do not work on 6.5, should work because `hash` makes an immutable table
(void
  (ann t* (HashTable Any Any))
  (ann t* (HashTable Symbol (U String Integer))))
