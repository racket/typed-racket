#;
(exn-pred "Domain: String String *")
#lang typed/racket

(: f (-> String String * String))
(define (f . xs)
    (apply string-append xs))
;(f "c" "d" "e" "f") ; succeeds
(apply f "c" 1) ; typecheck fail



