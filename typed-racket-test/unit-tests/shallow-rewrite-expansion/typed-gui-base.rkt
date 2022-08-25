#lang typed/racket/shallow

(require typed/racket/gui/base)

(let ()
  (get-face-list 'all) ;; no shape-check
  (get-top-level-windows) ;; no shape-check
  (find-graphical-system-path 'init-file) ;; no shape-check
  (system-position-ok-before-cancel?) ;; no shape-check
  (void))

