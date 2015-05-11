#lang racket/gui

;; make sure the first-order check for opaque object/c
;; can distinguish Pasteboard% and Text% objects

(module t typed/racket/gui
  (define snip (make-object string-snip% "foo"))
  (provide snip))

(require 't)

(define p (new pasteboard%))
(send p insert snip)
(send (send snip get-admin) get-editor)
