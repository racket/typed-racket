#lang typed/racket/base

;; Provides type bindings for racket/stream.

(require/typed/provide
  racket/stream
  [#:opaque StreamOf stream/c] ; TODO is this how this works?
  [#:opaque Stream stream?]
  [#:opaque StreamEmpty stream-empty?]
  [stream-first ((∩ Stream StreamEmpty) -> Any)]
  [stream-rest ((∩ Stream StreamEmpty) -> Stream)]
  ; TODO macros?
  ;[stream-cons ...]
  ;[stream-lazy ...]
  [stream-force (Stream -> Stream)]
  ; TODO macros?
  ;[stream ...]
  ;[stream* ...]
  [in-stream (Stream -> Stream)]
  ; TODO causes a contract violation
  ;[empty-stream Stream]
  [stream->list (Stream -> (Listof Any))]
  ; TODO causes a contract violation
  [stream-length (Stream -> Exact-Nonnegative-Integer)]
  [stream-ref (Stream Exact-Nonnegative-Integer -> Any)]
  [stream-tail (Stream Exact-Nonnegative-Integer -> Any)]
  [stream-take (Stream Exact-Nonnegative-Integer -> Any)]
  ; TODO macros?
  ;[stream-append (Stream ... -> Any)]
  [stream-map ((Any -> Any) Stream -> Stream)]
  ; TODO macros?
  ;[stream-andmap ((Any ... -> Boolean) Stream -> Stream)]
  ;[stream-ormap ((Any ... -> Boolean) Stream -> Stream)]
  ;[stream-for-each ((Any ... -> Any) Stream -> Void)]
  ;[stream-fold ((Any Any ... -> Any) Any Stream -> Any)]
  [stream-count ((Any -> Any) Stream -> Exact-Nonnegative-Integer)]
  ;[stream-filter ((Any ... -> Boolean) Stream -> Stream)]
  [stream-add-between (Stream Any -> Stream)]
  ; TODO for/stream is a a macro
  ; TODO
  ; - gen:stream
  ; - prop:stream
  )
