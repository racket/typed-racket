#lang typed/racket/base

;; a typed wrapper for the json library

;; To be robust against the json-null parameter,
;; the typed interface will always explicitly specify 'null.

(provide JSExpr
         (rename-out
          [jsexpr?* jsexpr?]
          [write-json* write-json]
          [read-json* read-json]
          [jsexpr->string* jsexpr->string]
          [jsexpr->bytes* jsexpr->bytes]
          [string->jsexpr* string->jsexpr]
          [bytes->jsexpr* bytes->jsexpr]))

(define-type JSExpr
  (U 'null Boolean String Integer Inexact-Real (Listof JSExpr) (HashTable Symbol JSExpr)))

(require/typed
 json
 [jsexpr? (-> Any #:null 'null Boolean)]
 [write-json (->* (JSExpr #:null 'null)
                  (Output-Port #:encode (U 'control 'all))
                  Any)]
 [read-json (->* (#:null 'null) (Input-Port) (U JSExpr EOF))]
 [jsexpr->string (-> JSExpr #:null 'null [#:encode (U 'control 'all)] String)]
 [jsexpr->bytes (-> JSExpr #:null 'null [#:encode (U 'control 'all)] Bytes)]
 [string->jsexpr (-> String #:null 'null JSExpr)]
 [bytes->jsexpr (-> Bytes #:null 'null JSExpr)])

(: jsexpr?* (-> Any Boolean))
(define (jsexpr?* v)
  (jsexpr? v #:null 'null))

(: write-json* (->* (JSExpr)
                    (Output-Port #:encode (U 'control 'all))
                    Any))
(define (write-json* js [out (current-output-port)]
                     #:encode [enc 'control])
  (write-json js out #:encode enc #:null 'null))

(: read-json* (->* () (Input-Port) (U JSExpr EOF)))
(define (read-json* [in (current-input-port)])
  (read-json in #:null 'null))

(: jsexpr->string* (-> JSExpr [#:encode (U 'control 'all)] String))
(define (jsexpr->string* js #:encode [enc 'control])
  (jsexpr->string js #:encode enc #:null 'null))

(: jsexpr->bytes* (-> JSExpr [#:encode (U 'control 'all)] Bytes))
(define (jsexpr->bytes* js #:encode [enc 'control])
  (jsexpr->bytes js #:encode enc #:null 'null))

(: string->jsexpr* (-> String JSExpr))
(define (string->jsexpr* v)
  (string->jsexpr v #:null 'null))

(: bytes->jsexpr* (-> Bytes JSExpr))
(define (bytes->jsexpr* v)
  (bytes->jsexpr v #:null 'null))


