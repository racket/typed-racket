#;
(exn-pred ".*found 7 fields in type, but 8 fields in struct pict.*")
#lang s-exp typed-racket/base-env/extra-env-lang

(require pict)

(type-environment
 [#:struct pict ([draw : Univ]
                 [width : -Real]
                 [height : -Real]
                 [ascent : -Real]
                 [children : Univ]
                 [panbox : Univ]
                 [last : Univ])
           #:extra-constructor-name make-pict])
