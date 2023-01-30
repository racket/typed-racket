#lang typed/racket/base

;; Provides type bindings for racket/stream.

(require/typed/provide
  racket/stream
  [#:opaque StreamOf stream/c] ; TODO is this how this works?
  [#:opaque Stream stream?]
  [#:opaque StreamEmpty stream-empty?]
  [stream-first ((∩ Stream StreamEmpty) -> Any)]
  [stream-rest ((∩ Stream StreamEmpty) -> Stream)]
  ; TODO macros
  ;[stream-cons ...]
  ;[stream-lazy ...]
  [stream-force (Stream -> Stream)]
  ; TODO macros
  ;[stream ...]
  ;[stream* ...]
  [in-stream (Stream -> Stream)]
  ; TODO causes a contract violation
  ;[empty-stream Stream]
  [stream->list (Stream -> (Listof Any))]
  [stream-length (Stream -> Exact-Nonnegative-Integer)]
  [stream-ref (Stream Exact-Nonnegative-Integer -> Any)]
  [stream-tail (Stream Exact-Nonnegative-Integer -> Any)]
  [stream-take (Stream Exact-Nonnegative-Integer -> Any)]
  [stream-append (->* () #:rest-star (Stream) Any)]
  [stream-map ((Any -> Any) Stream -> Stream)]
  [stream-andmap ((->* () #:rest-star (Any) Boolean) Stream -> Stream)]
  [stream-ormap ((->* () #:rest-star (Any) Boolean) Stream -> Stream)]
  [stream-for-each ((->* () #:rest-star (Any) Any) Stream -> Void)]
  [stream-fold ((->* (Any) #:rest-star (Any) Any) Any Stream -> Any)]
  [stream-count ((Any -> Any) Stream -> Exact-Nonnegative-Integer)]
  [stream-filter ((->* () #:rest-star (Any) Boolean) Stream -> Stream)]
  [stream-add-between (Stream Any -> Stream)]
  ; TODO for/stream is a a macro
  ; TODO
  ; - gen:stream
  ; - prop:stream
  )
