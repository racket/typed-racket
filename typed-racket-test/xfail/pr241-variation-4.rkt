#lang racket

;; Wrapping opaque structs should succeed

;; From Issue #203
;;   https://github.com/racket/typed-racket/issues/203

(require typed-racket/utils/any-wrap
         typed-racket/utils/inspector)
(current-inspector new-inspector)
(struct s ())
(contract any-wrap/c (s) 'a 'b)
