#lang typed/racket/base

;; Test types for framework `get-map-function-table/ht`

(require typed/framework typed/racket/class)

(: km (Instance Keymap:Aug-Keymap<%>))
(define km
  (cast (keymap:get-global) (Instance Keymap:Aug-Keymap<%>)))

(void
  (ann
    (send km get-map-function-table)
    (HashTable Symbol String))

  ;; Inserts bindings into argument
  (ann
    (send km get-map-function-table/ht (ann (make-hash) (HashTable Symbol String)))
    (Mutable-HashTable Symbol String)))
