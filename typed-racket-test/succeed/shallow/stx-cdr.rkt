#lang typed/racket/base/shallow
(require typed/syntax/stx)

;; ... no idea whats wrong
;; - guarded never fails so far
;; - shallow passes with no cod-check
;;   when type says (List ....)
;;   bun NOT when (Listof ....)

;(: x (Syntaxof (Pairof Identifier (List Identifier))))
(: x (Syntaxof (Pairof Identifier (Listof Identifier))))
(define x #'(a b))
x
;;(: y (List Identifier))
(: y (Listof Identifier))
(define y (stx-cdr x))
y
(and (list? y) (= 1 (length y)))

