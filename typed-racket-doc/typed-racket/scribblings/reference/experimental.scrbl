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



@section{Refinements and Linear Integer Reasoning}

Refinement types have been added to Typed Racket's core, but
Typed Racket does not yet have function types which allow
dependencies between argument types, limiting how useful
refinements are for now. Allowing argument dependencies
is on our `to do' list.

@defform[#:literals (Refine : Top Bot ! and or when
                            unless if < <= = > >= car cdr
                            vector-length + *)
         (Refine [id : type] proposition)
         #:grammar
         ([proposition Top
           Bot
           (: symbolic-object type)
           (! symbolic-object type)
           (and proposition ...)
           (or proposition ...)
           (when proposition proposition)
           (unless proposition proposition)
           (if proposition proposition proposition)
           (linear-comp symbolic-object symbolic-object)]
          [linear-comp < <= = >= >]
          [symbolic-object exact-integer
           linear-term
           (+ linear-term linear-term ...)]
          [linear-term symbolic-path
           (* exact-integer symbolic-path)]
          [symbolic-path id
           (path-elem symbolic-path)]
          [path-elem car
           cdr
           vector-length])]{@racket[(Refine [v : t] p)] is a
 refinement of type @racket[t] with logical proposition
 @racket[p], or in other words it describes any value
 @racket[v] of type @racket[t] for which the logical
 proposition @racket[p] holds.

 @ex[(ann 42 (Refine [n : Integer] (= n 42)))]

 Note: The identifier in a refinement type is in scope
 inside the proposition, but not the type.

}


@racket[(: o t)] used as a proposition holds when symbolic object @racket[o]
is of type @racket[t.]

@defform[(! sym-obj type)]{This is the dual of @racket[(: o t)], holding
when @racket[o] is not of type @racket[t].}

Propositions can also describe linear inequalities (e.g. @racket[(<= x 42)]
holds when @racket[x] is less than or equal to @racket[42]), using any
of the following relations: @racket[<=], @racket[<], @racket[=],
@racket[>=], @racket[>].

The following logical combinators hold as one would expect
depending on which of their subcomponents hold hold:
@racket[and], @racket[or], @racket[if], @racket[not].

@racket[(when p q)] is equivalent to @racket[(or (not p) (and p q))].

@racket[(unless p q)] is equivalent to @racket[(or p q)].

Typed Racket's linear integer reasoning is turned off by
default. If you want to activate it, you must add the
@racket[#:with-linear-integer-arithmetic] keyword when
specifying the language of your program:


@racketmod[typed/racket #:with-linear-integer-arithmetic]


With this language option on, code such as the following
will type check:

@racketblock[(if (< 5 4)
                 (+ "Luke," "I am your father")
                 "that's impossible!")]

i.e. with linear integer reasoning enabled, Typed Racket
detects that the comparison is guaranteed to produce
@racket[#f], and thus the clearly ill-typed `then'-branch is
ignored by the type checker since it is guaranteed to be
dead code.
