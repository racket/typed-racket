#lang typed/racket #:with-refinements

;; although the test expression will always be true
;; since the type for <= can only express finitely
;; many cases, it is unreasonable for us to get the
;; detailed linear integer information out of all use cases

;; in particular, here is an example we will likely fail,
;; since we do not bother to include the type for <=
;; that includes linear integer propositions when there are
;; 10 arguments
(if (<= 1 2 3 4 5 6 7 8 9 10)
    (void)
    (+ "dead" "code"))