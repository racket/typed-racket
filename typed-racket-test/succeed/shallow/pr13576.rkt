#lang typed/racket/base/shallow

(define: (A ...) (lister args : A ... A) : (List A ... A)
   args)
