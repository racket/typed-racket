#lang typed/racket/base

(require/typed/provide
 rackunit/docs-complete
 [check-docs (Symbol
              [#:skip (U Regexp
                         Symbol
                         (Listof (U Regexp Symbol))
                         (Symbol -> Any)
                         #f)]
              -> Any)])
