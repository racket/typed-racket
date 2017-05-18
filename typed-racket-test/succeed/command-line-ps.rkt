#lang typed/racket

(define *message* : (Parameterof (Listof String)) (make-parameter '()))
(define *verbose* : (Parameterof Boolean) (make-parameter #f))

(define (parse-cmdline)
    (command-line
     #:program "q"
     #:once-each
     [("-v" "--verbose") "verbose mode" (*verbose* #t)]
     #:ps "foo bar" ; <---- causing type error
     #:args #{msg : String} (*message* msg)))

