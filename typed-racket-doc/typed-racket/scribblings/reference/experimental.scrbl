#lang scribble/manual

@begin[(require "../utils.rkt" scribble/example)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@(define the-top-eval (make-base-eval #:lang 'typed/racket))
@(define-syntax-rule (ex . args)
   (examples #:eval the-top-eval . args))


@title{Experimental Features}

These features are currently experimental and subject to change.

@defform[(declare-refinement id)]{Declares @racket[id] to be usable in
refinement types.}

@defform[(Refinement id)]{Includes values that have been tested with the
predicate @racket[id], which must have been specified with
@racket[declare-refinement].}

@defform[(define-typed-struct/exec forms ...)]{Defines an executable structure.}

@defform[(define-new-subtype name (constructor t))]{
Defines a new type @racket[name] that is a subtype of @racket[t].
The @racket[constructor] is defined as a function that takes a value of type
@racket[t] and produces a value of the new type @racket[name].
A @racket[define-new-subtype] definition is only allowed at the top level of a
file or module.

This is purely a type-level distinction, with no way to distinguish the new type
from the base type at runtime. Predicates made by @racket[make-predicate]
won't be able distinguish them properly, so they will return true for all values
that the base type's predicate would return true for. This is usually not what
you want, so you shouldn't use @racket[make-predicate] with these types.

@ex[(module m typed/racket
      (provide Radians radians f)
      (define-new-subtype Radians (radians Real))
      (: f : [Radians -> Real])
      (define (f a)
        (sin a)))
    (require 'm)
    (radians 0)
    (f (radians 0))]
}
