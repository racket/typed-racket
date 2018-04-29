#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang racket

;; Ensure contracts for inner work correctly

(module t typed/racket/shallow
  (provide c%)
  (define c%
    (class object%
      (super-new)
      (: m (-> Void) #:augment (-> String))
      (define/pubment (m) (inner "hi" m) (void)))))

(require (submod "." t))
(send (new (class c%
             (super-new)
             (define/augment (m) 'not-a-string)))
      m)
