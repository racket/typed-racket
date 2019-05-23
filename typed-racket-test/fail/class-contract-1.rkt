#;
(exn-pred #rx"expected: String.*given: 'not-a-string")
#lang racket

;; Ensure contracts for inner work correctly

(module t typed/racket
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
