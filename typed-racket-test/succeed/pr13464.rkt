#lang racket
(require racket/runtime-path)
(define-runtime-path p "pr13464.rktl")
(current-namespace (make-base-namespace))
(load p)
