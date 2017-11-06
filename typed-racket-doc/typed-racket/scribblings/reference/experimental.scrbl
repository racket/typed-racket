#lang scribble/manual

@begin[(require "../utils.rkt" scribble/example)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@(define the-top-eval (make-base-eval #:lang 'typed/racket))
@(define-syntax-rule (ex . args)
   (examples #:eval the-top-eval . args))


@title{Experimental Features}

These features are currently experimental and subject to change.

@defform[(declare-refinement id)]{Declares @racket[id] to be usable in
@racket[Refinement] types.}

@defform[(Refinement id)]{Includes values that have been tested with the
predicate @racket[id], which must have been specified with
@racket[declare-refinement]. These predicate-based refinements are distinct
from Typed Racket's more general @racket[Refine] form.}

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



@section{Logical Refinements and Linear Integer Reasoning}

Typed Racket allows types to be `refined' or `constrained'
by logical propositions. These propositions can mention
certain program terms, allowing a program's types to depend
on the values of terms.

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
           symbolic-path
           (+ symbolic-object ...)
           (- symbolic-object ...)
           (* exact-integer symbolic-object)]
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

In addition to reasoning about propositions regarding types
(i.e. something is or is not of some particular type), Typed
Racket is equipped with a linear integer arithmetic solver
that can prove linear constraints when necessary. To turn on
this solver (and some other refinement reasoning), you must add
the @racket[#:with-refinements] keyword when
specifying the language of your program:


@racketmod[typed/racket #:with-refinements]


With this language option on, type checking the following
primitives will produce more specific logical info (when they are being
applied to 2 or 3 arguments): @racket[*], @racket[+], @racket[-],
@racket[<], @racket[<=], @racket[=], @racket[>=], @racket[>],
and @racket[make-vector].

This allows code such as the following to type check:

@racketblock[(if (< 5 4)
                 (+ "Luke," "I am your father")
                 "that's impossible!")]

i.e. with refinement reasoning enabled, Typed Racket
detects that the comparison is guaranteed to produce
@racket[#f], and thus the clearly ill-typed `then'-branch is
ignored by the type checker since it is guaranteed to be
dead code.


@section{Dependent Function Types}

Typed Racket supports explicitly dependent function types:
  
@defform*/subs[#:link-target? #f #:id -> #:literals (:)
               [(-> ([id : opt-deps arg-type] ...)
                    opt-pre
                    range-type
                    opt-props)]
               ([opt-deps (code:line) (id ...)]
                [opt-pre (code:line)
                 (code:line #:pre (id ...) prop)]
                [opt-props (code:line)
                 (code:line opt-pos-prop opt-neg-prop opt-obj)]
                [opt-pos-prop (code:line)
                 (code:line #:+ prop)]
                [opt-neg-prop (code:line)
                 (code:line #:- prop)]
                [opt-obj (code:line)
                 (code:line #:object obj)])]


The syntax is similar to Racket's dependent contracts syntax
(i.e. @racket[->i]).

Each function argument has a name, an optional list of
identifiers it depends on, an argument type. An
argument's type can mention (i.e. depend on) other arguments
by name if they appear in its list of dependencies.
Dependencies cannot be cyclic.

A function may have also have a precondition. The
precondition is introduced with the @racket[#:pre] keyword
followed by the list of arguments on which it depends
and the proposition which describes the precondition.

A function's range may depend on any of its arguments.

The grammar of supported propositions and symbolic objects
(i.e. @racket[prop] and @racket[obj]) is the same as
the @racket[proposition] and @racket[symbolic-object] grammars
from @racket[Refine]'s syntax.

For example, here is a dependently typed version of
Racket's @racket[vector-ref] which eliminates vector
bounds errors during type checking instead of at run time:

@ex[#:label #f
 (require racket/unsafe/ops)

 (: safe-ref1 (All (A) (-> ([v : (Vectorof A)]
                            [n : (v) (Refine [i : Natural]
                                             (< i (vector-length v)))])
                           A)))
 (define (safe-ref1 v n) (unsafe-vector-ref v n))
 (safe-ref1 (vector "safe!") 0)
 (eval:error (safe-ref1 (vector "not safe!") 1))]

Here is an equivalent type that uses a precondition instead of a
refinement type:

@ex[#:label #f
 (: safe-ref2 (All (A) (-> ([v : (Vectorof A)]
                            [n : Natural])
                           #:pre (v n) (< n (vector-length v))
                           A)))
 (define (safe-ref2 v n) (unsafe-vector-ref v n))
 (safe-ref2 (vector "safe!") 0)
 (eval:error (safe-ref2 (vector "not safe!") 1))]

Using preconditions can provide more detailed type checker
error messages, i.e. they can indicate when the arguments
were of the correct type but the precondition could not
be proven.
