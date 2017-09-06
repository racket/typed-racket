#lang scribble/manual
@require[scribble/example (for-label racket/base racket/contract)]

@title{Source Syntax}

@defmodule[syntax/source-syntax]{}

@defproc[(recover-source-syntax [orig syntax?]
                                [expanded syntax?]
                                [#:traverse-now? now? boolean? #f])
         (-> syntax? (or/c syntax? #f))]{
  Return a procedure that accepts a syntax object from @racket[expanded]
  and returns the outermost syntax object in @racket[orig] that has the same
  location as the given syntax object.
  If no syntax object in @racket[orig] has the same location as the given syntax
  object, the procedure repeats with the parent of the given syntax object.
}

@examples[#:eval (make-base-eval '(require syntax/source-syntax))
  (let* ([orig #'(λ (x [y 0]) (+ x y))]
         [expanded (expand orig)]
         [recovered ((recover-source-syntax orig expanded) expanded)])
    (syntax? recovered))
]
