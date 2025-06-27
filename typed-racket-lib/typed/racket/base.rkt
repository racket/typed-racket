#lang typed-racket/minimal

(providing (libs (except racket/base #%module-begin #%top-interaction
                         with-handlers with-handlers* default-continuation-prompt-tag
                         define λ lambda define-struct for for*
                         let let* let-values let*-values letrec letrec-values
                         let/cc let/ec do case-lambda case-λ struct
                         for/list for/vector for/hash for/hasheq for/hasheqv for/hashalw
                         for/and for/or for/sum for/product for/lists
                         for/first for/last for/fold for/foldr for*/list for*/lists
                         for*/vector for*/hash for*/hasheq for*/hasheqv for*/hashalw
                         for*/and
                         for*/or for*/sum for*/product for*/first for*/last
                         for*/fold for*/foldr))
           (basics #%module-begin #%top-interaction)
           (ts-except with-type-shallow with-type-optional))

(require racket/deprecation
         typed-racket/base-env/extra-procs
         (except-in typed-racket/base-env/prims
                    require-typed-struct-legacy
                    require/typed-legacy
                    require-typed-signature)
         typed-racket/base-env/base-types
         (except-in typed-racket/base-env/base-types-extra Distinction Unit))
(provide define-type
         (all-from-out typed-racket/base-env/prims)
         (all-from-out typed-racket/base-env/base-types)
         (all-from-out typed-racket/base-env/base-types-extra)
         assert defined? with-type for for*
         case-lambda:
         struct:
         define-struct:
         define-typed-struct
         define-struct/exec:
         for:
         for/and:
         for/first:
         for/flvector:
         for/extflvector:
         for/fold:
         for/foldr:
         for/hash:
         for/hasheq:
         for/hasheqv:
         for/hashalw:
         for/last:
         for/list:
         for/lists:
         for/set:
         for/or:
         for/product:
         for/sum:
         for/vector:
         for/and:
         for*/first:
         for*/flvector:
         for*/extflvector:
         for*/fold:
         for*/foldr:
         for*/hash:
         for*/hasheq:
         for*/hasheqv:
         for*/hashalw:
         for*/last:
         for*/list:
         for*/lists:
         for*/set:
         for*/or:
         for*/product:
         for*/sum:
         for*/vector:
         do:
         define-type-alias
         define-typed-struct
         Un
         mu
         Tuple
         Parameter
         Pair)


(define-deprecated-alias case-lambda: case-lambda)
(define-deprecated-alias struct: struct)
(define-deprecated-alias define-struct: define-struct)
(define-deprecated-alias define-typed-struct define-struct)
(define-deprecated-alias define-struct/exec: define-struct/exec)
(define-deprecated-alias for: for)
(define-deprecated-alias for/and: for/and)
(define-deprecated-alias for/first: for/first)
(define-deprecated-alias for/flvector: for/flvector)
(define-deprecated-alias for/extflvector: for/extflvector)
(define-deprecated-alias for/fold: for/fold)
(define-deprecated-alias for/foldr: for/foldr)
(define-deprecated-alias for/hash: for/hash)
(define-deprecated-alias for/hasheq: for/hasheq)
(define-deprecated-alias for/hasheqv: for/hasheqv)
(define-deprecated-alias for/hashalw: for/hashalw)
(define-deprecated-alias for/last: for/last)
(define-deprecated-alias for/list: for/list)
(define-deprecated-alias for/lists: for/lists)
(define-deprecated-alias for/set: for/set)
(define-deprecated-alias for/or: for/or)
(define-deprecated-alias for/product: for/product)
(define-deprecated-alias for/sum: for/sum)
(define-deprecated-alias for/vector: for/vector)
(define-deprecated-alias for/and: for/and)
(define-deprecated-alias for*/first: for*/first)
(define-deprecated-alias for*/flvector: for*/flvector)
(define-deprecated-alias for*/extflvector: for*/extflvector)
(define-deprecated-alias for*/fold: for*/fold)
(define-deprecated-alias for*/foldr: for*/foldr)
(define-deprecated-alias for*/hash: for*/hash)
(define-deprecated-alias for*/hasheq: for*/hasheq)
(define-deprecated-alias for*/hasheqv: for*/hasheqv)
(define-deprecated-alias for*/hashalw: for*/hashalw)
(define-deprecated-alias for*/last: for*/last)
(define-deprecated-alias for*/list: for*/list)
(define-deprecated-alias for*/lists: for*/lists)
(define-deprecated-alias for*/set: for*/set)
(define-deprecated-alias for*/or: for*/or)
(define-deprecated-alias for*/product: for*/product)
(define-deprecated-alias for*/sum: for*/sum)
(define-deprecated-alias for*/vector: for*/vector)
(define-deprecated-alias do: do)
(define-deprecated-alias define-type-alias define-type)
(define-deprecated-alias define-typed-struct define-struct)
(define-deprecated-alias Un U)
(define-deprecated-alias mu Rec)
(define-deprecated-alias Tuple List)
(define-deprecated-alias Parameter Parameterof)
(define-deprecated-alias Pair Pairof)
