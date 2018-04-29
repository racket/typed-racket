#;
(exn-pred exn:fail:contract?)
#lang scheme

(require typed/racket/shallow)

(let ([x 'hello])
  (with-type
   #:result String
   #:freevars ([x String])
   (string-append x ", world")))
