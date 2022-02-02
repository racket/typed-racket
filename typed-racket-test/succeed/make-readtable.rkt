#lang typed/racket

(define-namespace-anchor anchor)

(parameterize ([current-readtable
                (make-readtable
                 (current-readtable)
                 #\, #\space #f
                 #\~ #\, (current-readtable)
                 #\✓ 'dispatch-macro (const #t)
                 #\✕ 'dispatch-macro (const #f))])
  (eval
   (with-input-from-string
     "`(+ ~@(list 1 2 3) #\"xyz\" #✓ #✕)"
     read)
   (namespace-anchor->namespace anchor)))
