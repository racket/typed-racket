#lang typed/racket/base

(require (only-in ffi/unsafe ffi-lib?))
(define-type FFILib (Opaque ffi-lib?))
(provide FFILib)

(require/typed/provide openssl/libcrypto
  [libcrypto (Option FFILib)]
  [libcrypto-load-fail-reason (Option String)]
  [openssl-lib-versions (Listof String)]
  )
