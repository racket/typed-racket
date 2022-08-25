#lang typed/racket/base/shallow

;; default-continuation-prompt-tag is separate from the base environment,
;;  so double-check that it does not get a codomain check
;;
;; to test, expand this module and make sure there's no "shape-check"

(default-continuation-prompt-tag)
