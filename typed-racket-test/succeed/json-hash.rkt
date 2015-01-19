#lang typed/racket/base
(require typed/json)
(: jsx JSExpr)
(define jsx #hasheq((a . "val1") (b . "val2") (c . "val3")))
