#lang typed/racket/base


(require/typed
 racket/struct
 [make-constructor-style-printer
  (All (A) (-> (-> A (U Symbol String))
               (-> A (Sequenceof Any))
               (-> A Output-Port (U #t #f 0 1) Void)))])


((make-constructor-style-printer
  (lambda ([x : String]) x)
  (lambda ([x : String]) null))
 ""
 (open-output-string) #t)
