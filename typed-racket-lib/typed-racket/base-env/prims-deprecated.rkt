#lang racket/base


(provide case-lambda:
         struct:
         define-struct:
         define-typed-struct
         define-struct/exec:
         for:
         for/and:
         for/first:
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
         for*/and:
         for*/first:
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
         define-type-alias)


(require racket/deprecation
         typed-racket/base-env/prims)


(define-deprecated-alias define-type-alias define-type)
(define-deprecated-alias case-lambda: case-lambda)
(define-deprecated-alias struct: struct)
(define-deprecated-alias define-struct: define-struct)
(define-deprecated-alias define-typed-struct define-struct)
(define-deprecated-alias define-struct/exec: define-struct/exec)
(define-deprecated-alias for: for)
(define-deprecated-alias for/and: for/and)
(define-deprecated-alias for/first: for/first)
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
(define-deprecated-alias for*/and: for/and)
(define-deprecated-alias for*/first: for*/first)
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
