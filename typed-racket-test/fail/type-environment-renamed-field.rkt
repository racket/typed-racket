#;
(exn-pred ".*expected field name 4 to be descent, but found ascent.*")
#lang s-exp typed-racket/base-env/extra-env-lang

(require pict)

(type-environment
 [#:struct pict ([draw : Univ]
                 [width : -Real]
                 [height : -Real]
                 [ascent : -Real]
                 [ascent : -Real]
                 [children : Univ]
                 [panbox : Univ]
                 [last : Univ])
           #:extra-constructor-name make-pict])

