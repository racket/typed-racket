#lang typed/racket/gui

(require/typed racket/gui/base
               [editor-canvas% Editor-Canvas%]
               [canvas% Canvas%])

(define frame (new frame% [label "Example"]))
(define canvas (new canvas% [parent frame]))
(define editor-canvas (new editor-canvas% [parent frame]))
