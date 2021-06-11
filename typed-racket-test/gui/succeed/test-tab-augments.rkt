#lang typed/racket/gui

(define frame (new frame% [label "Example"]))
(define-type Tab-Panel-With-Callbacks%
  (Class #:implements/inits Tab-Panel%
         (init-field [on-reorder-callback ((Listof Exact-Nonnegative-Integer) -> Void)]
                     [on-close-request-callback (Exact-Nonnegative-Integer -> Void)])))
(define tab-panel-with-callbacks% : Tab-Panel-With-Callbacks%
  (class tab-panel%
    (super-new)
    (init-field on-reorder-callback
                on-close-request-callback)
    (define/augment (on-reorder tab-list)
      (on-reorder-callback tab-list))
    (define/augment (on-close-request index)
      (on-close-request-callback index))))
(define tabs : (Instance Tab-Panel-With-Callbacks%)
  (new tab-panel-with-callbacks%
       [choices '("one" "two" "three")]
       [parent frame]
       [style '(can-reorder can-close no-border)]
       [on-reorder-callback (lambda (tab-list)
                              (displayln (format "on-reorder ~v" tab-list)))]
       [on-close-request-callback
        (lambda (tabs)
          (displayln (format "on-close-request ~v" tabs)))]))
