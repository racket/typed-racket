#lang typed/racket/base

(provide (all-defined-out))

(require "base.rkt")

(define-type SQLite3-Database-Storage (U Path-String 'memory 'temporary))
(define-type SQLite3-Connection-Mode (U 'read-only 'read/write 'create))

(require/typed/provide db/sqlite3 [sqlite3-available? (-> Boolean)])

(require/db/provide (->* (#:database SQLite3-Database-Storage)
                         (#:mode SQLite3-Connection-Mode
                          #:busy-retry-limit (U Natural +inf.0)
                          #:busy-retry-delay Nonnegative-Real
                          #:use-place Boolean)
                         sqlite3))
