#lang typed/racket/base

;; a typed wrapper for the json library

(provide JSExpr)

(define-type JSExpr
  (U 'null Boolean String Integer Inexact-Real (Listof JSExpr) (HashTable Symbol JSExpr)))

(require/typed/provide json
                       [jsexpr? (Any -> Boolean)]
                       [write-json (->* (JSExpr)
                                        (Output-Port #:encode (U 'control 'all))
                                        Any)]
                       [read-json (->* () (Input-Port) (U JSExpr EOF))]
                       [jsexpr->string (JSExpr [#:encode (U 'control 'all)] -> String)]
                       [jsexpr->bytes (JSExpr [#:encode (U 'control 'all)] -> Bytes)]
                       [string->jsexpr (String -> JSExpr)]
                       [bytes->jsexpr (Bytes -> JSExpr)])
