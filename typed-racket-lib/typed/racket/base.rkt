#lang typed-racket/minimal

(providing (libs (except racket/base #%module-begin #%top-interaction
                         with-handlers with-handlers* default-continuation-prompt-tag
                         define λ lambda define-struct for for*
                         let let* let-values let*-values letrec letrec-values
                         let/cc let/ec do case-lambda struct
                         for/list for/vector for/hash for/hasheq for/hasheqv
                         for/and for/or for/sum for/product for/lists
                         for/first for/last for/fold for/foldr for*/list for*/lists
                         for*/vector for*/hash for*/hasheq for*/hasheqv for*/and
                         for*/or for*/sum for*/product for*/first for*/last
                         for*/fold for*/foldr))
           (basics #%module-begin #%top-interaction))

(require typed-racket/base-env/extra-procs
         (except-in typed-racket/base-env/prims
           require-typed-struct-legacy
           require/typed-legacy
           require-typed-signature)
         typed-racket/base-env/base-types
         (except-in typed-racket/base-env/base-types-extra Distinction Unit))
(provide (rename-out [define-type-alias define-type])
         (all-from-out typed-racket/base-env/prims)
         (all-from-out typed-racket/base-env/base-types)
         (all-from-out typed-racket/base-env/base-types-extra)
         assert defined? with-type for for*)
