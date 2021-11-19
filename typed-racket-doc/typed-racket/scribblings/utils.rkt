#lang at-exp scheme

(require scribble/manual scribble/core)
(provide (all-defined-out))

(define (item* header . args) (apply item @bold[header]{: } args))

(define-syntax-rule (tmod forms ...) (racketmod typed-scheme forms ...))

(define (gtech . x)
  (apply tech x #:doc '(lib "scribblings/guide/guide.scrbl")))

(define (rtech . x)
  (apply tech x #:doc '(lib "scribblings/reference/reference.scrbl")))

(define (tr-guide-secref tag)
  (secref tag #:doc '(lib "typed-racket/scribblings/ts-guide.scrbl")))

(define (tr-reference-secref tag)
  (secref tag #:doc '(lib "typed-racket/scribblings/ts-reference.scrbl")))

(define (tr-reference-seclink tag . content)
  (apply seclink tag content #:doc '(lib "typed-racket/scribblings/ts-reference.scrbl")))

(define (r-guide-secref tag)
  (secref tag #:doc '(lib "scribblings/guide/guide.scrbl")))

(define (r-reference-secref tag)
  (secref tag #:doc '(lib "scribblings/reference/reference.scrbl")))

(define ** (let ([* #f]) @racket[*]))

(define-syntax-rule (annvar x t)
  (make-element #f (list @racketparenfont["#{"]
                         @racket[x : t]
                         @racketparenfont["}"])))

(define-syntax-rule (annexpr x t)
  (make-element #f (list @racketparenfont["#{"]
                         @racket[x :: t]
                         @racketparenfont["}"])))

(define-syntax-rule (defalias id1 id2 kind)
  @defidform[#:kind kind id1]{An alias for @racket[id2].})

(define-syntax-rule (deftypeconstr args ...)
  @defform[#:kind "type constructor" args ...])

(define-syntax-rule (deftypeconstr* args ...)
  @defform*[#:kind "type constructor" args ...])

(define-syntax-rule (deftypeconstr*/subs args ...)
  @defform*/subs[#:kind "type constructor" args ...])

(define-syntax-rule (deftype args ...)
  @defidform[#:kind "type" args ...])

(define-syntax-rule (deftypeform args ...)
  @defform[#:kind "type" args ...])

(define-syntax-rule (deftypeform* args ...)
  @defform*[#:kind "type" args ...])

(define-syntax-rule (deftypeform/none args ...)
  @defform/none[#:kind "type" args ...])

(define-syntax-rule (deftypeconstr/none args ...)
  @defform/none[#:kind "type constructor" args ...])
