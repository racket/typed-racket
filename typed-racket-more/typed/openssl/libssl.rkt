#lang typed/racket/base

(require "libcrypto.rkt")

(require/typed/provide openssl/libssl
  [libssl (Option FFILib)]
  [libssl-load-fail-reason (Option String)]
  )
