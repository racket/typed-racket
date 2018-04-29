#lang typed/racket/base/shallow

;; Struct predicates don't need a typecheck
;;
;; Expand this file, jump to the bottom,
;;  look for ONE shape-check around `a?`

(module a racket/base
  (provide (struct-out a))
  (struct a [x]))

(require/typed 'a
  (#:struct a ([x : Symbol])))

(struct b ((y : Symbol)))

(a? #f) ;; require/typed struct
(b? #f) ;; TR struct
(exn? #f) ;; builtin struct
