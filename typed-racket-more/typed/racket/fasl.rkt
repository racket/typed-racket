#lang typed/racket/base

(require/typed/provide
 racket/fasl
 [s-exp->fasl (case-> [-> Any Bytes]
                      [-> Any Output-Port Void])]
 [fasl->s-exp (-> (U Input-Port Bytes) Any)])
