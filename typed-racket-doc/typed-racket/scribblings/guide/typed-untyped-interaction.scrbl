#lang scribble/manual

@(require "../utils.rkt"
          scribble/example
          (for-label (only-meta-in 0 typed/racket)))

@(define the-eval (make-base-eval))
@(the-eval '(require typed/racket))
@(define shallow-eval (make-base-eval #:lang 'typed/racket/shallow))
@(define optional-eval (make-base-eval #:lang 'typed/racket/optional))


@title[#:tag "typed-untyped-interaction"]{Typed-Untyped Interaction}

In the previous sections, all of the examples have consisted of programs
that are entirely typed. One of the key features of Typed Racket is that
it allows the combination of both typed and untyped code in a single
program.

From a static typing perspective, combining typed and untyped code is
straightforward.
Typed code must declare types for its untyped imports to let the type checker
validate their use (@secref{untyped-in-typed}).
Untyped code can freely import bindings from typed code (@secref{typed-in-untyped}).

At run-time, combining typed and untyped code is complicated because there is a
tradeoff between strong type guarantees and the performance cost of checking
that untyped code matches the types.
Typed Racket provides strong @emph{Deep} type guarantees by default, but offers two
weaker options as well: Shallow and Optional types
(@secref{protecting-interaction}).


@section[#:tag "untyped-in-typed"]{Using Untyped Code in Typed Code}

Suppose that we write the untyped module from @secref["quick"] again:

@racketmod[#:file "distance.rkt"
racket

(provide (struct-out pt)
         distance)

(struct pt (x y))

(code:contract distance : pt pt -> real)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-y p1))))))
]

If we want to use the @racket[_distance] function defined in the above module
from a typed module, we need to use the @racket[require/typed] form to import
it. Since the untyped module did not specify any types, we need to annotate
the imports with types (just like how the example in @secref["quick"] had
additional type annotations with @racket[:]):

@margin-note{Note that a typed module @emph{should not} use @racket[require/typed]
to import from another typed module. The @racket[require] form will work in such
cases.}

@racketmod[#:file "client.rkt"
typed/racket

(require/typed "distance.rkt"
               [#:struct pt ([x : Real] [y : Real])]
               [distance (-> pt pt Real)])

(distance (pt 3 5) (pt 7 0))
]

The @racket[require/typed] form has several kinds of clauses. The
@racket[#:struct] clause specifies the import of a structure type
and allows us to use the structure type as if it were defined with
Typed Racket's @racket[struct].

The second clause in the example above specifies that a given
binding @racket[_distance] has the given type @racket[(-> pt pt Real)].

Note that the @racket[require/typed] form can import bindings
from any module, including those that are part of the Racket standard
library. For example,

@racketmod[
typed/racket

(require/typed racket/base [add1 (-> Integer Integer)])
]

is a valid use of the @racket[require/typed] form and imports @racket[add1]
from the @racketmodname[racket/base] library.


@subsection{Opaque Types}

The @racket[#:opaque] clause of @racket[require/typed] defines a new type
 using a predicate from untyped code.
Suppose we have an untyped distance function that uses
 pairs of numbers as points:

@racketmod[#:file "distance2.rkt"
racket

(provide point?
         distance)

(code:contract A Point is a (cons real real))
(define (point? x)
  (and (pair? x)
       (real? (car x))
       (real? (cdr x))))

(code:contract distance : Point Point -> real)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (car p2) (car p1)))
           (sqr (- (cdr p2) (cdr p1))))))
]

A typed module can use @racket[#:opaque] to define a @racket[Point] type as
 all values that the @racket[point?] predicate returns @racket[#t] for:

@racketmod[#:file "client2.rkt"
typed/racket

(require/typed "distance2.rkt"
               [#:opaque Point point?]
               [distance (-> Point Point Real)])

(define p0 : Point (assert (cons 3 5) point?))
(define p1 : Point (assert (cons 7 0) point?))
(distance p0 p1)
]



@section[#:tag "typed-in-untyped"]{Using Typed Code in Untyped Code}

In the previous subsection, we saw that the use of untyped code from
typed code requires the use of @racket[require/typed]. However, the
use of code in the other direction (i.e., the use of typed code from
untyped code) requires no additional work.

If an untyped module @racket[require]s a typed module, it will be able
to use the bindings defined in the typed module as expected. The major
exception to this rule is that macros defined in typed modules may not
be used in untyped modules.


@section[#:tag "protecting-interaction"]{Protecting Typed-Untyped Interaction}

One might wonder if the interactions described in the first two
subsections are actually safe. After all, untyped code might be able to
ignore the errors that Typed Racket's type system will catch at
compile-time.

For example, suppose
that we write an untyped module that implements an @racket[_increment]
function:

@examples[#:eval shallow-eval #:hidden (module increment racket (provide increment) (code:contract increment : exact-integer? -> exact-integer?) (define (increment x) "this is broken"))]
@examples[#:eval optional-eval #:hidden (module increment racket (provide increment) (code:contract increment : exact-integer? -> exact-integer?) (define (increment x) "this is broken"))]
@examples[#:eval the-eval
(module increment racket
  (provide increment)

  (code:contract increment : exact-integer? -> exact-integer?)
  (define (increment x) "this is broken"))]

and a typed module that uses it:

@examples[#:label #f #:eval the-eval
(module client typed/racket

  (require/typed 'increment [increment (-> Integer Integer)])

  (increment 5))
]

This combined program has a problem. All uses of @racket[_increment]
in Typed Racket are correct under the assumption that the
@racket[_increment] function upholds the @racket[(-> Integer Integer)]
type. Unfortunately, our @racket[_increment] implementation does not
actually uphold this assumption, because the function actually produces
strings.

By default, Typed Racket establishes contracts wherever typed and untyped code
interact to ensure strong types.
These contracts can, however, have a non-trivial performance impact.
For programs in which these costs are problematic, Typed Racket provides
two alternatives. All together, the three options are Deep, Shallow, and Optional types.

@itemlist[#:style 'ordered
  @item{
    @emph{Deep} types get enforced with comprehensive contract checks.
  }
  @item{
    @emph{Shallow} types get checked in typed code with lightweight assertions
    called @emph{shape checks}.
  }
  @item{
    @emph{Optional} types do not get enforced in any way. They do not ensure
    safe typed-untyped interactions.
  }
]

@margin-note{See also: @secref["behavior-of-types" #:doc '(lib
"typed-racket/scribblings/ts-reference.scrbl")] in the Typed Racket Reference.}
The next subsections give examples of Deep, Shallow, and Optional behaviors.


@subsection{Deep Types: Completely Reliable}

When the @racket[_client] program above is run, standard Typed
Racket (aka. Deep Typed Racket) enforces the @racket[require/typed]
interface with a contract.
This contract detects a failed type assumption when the @racket[_client]
calls the untyped @racket[_increment] function:

@examples[#:label #f #:eval the-eval (eval:error (require 'client))]

Because the implementation in the untyped module broke the contract
by returning a string instead of an integer, the error message
@emph{blames} it.

@margin-note{For general information on Racket's contract system,
see @secref[#:doc '(lib "scribblings/guide/guide.scrbl")]{contracts}.}
In general, Deep Typed Racket checks all functions and other values
that pass from a typed module to untyped module or vice versa with
contracts. This means that, for example, Typed Racket can safely optimize
programs (see @secref["optimization"]) with the assurance that the program
will not segfault due to an unchecked assumption.

@bold{Important caveat}: contracts such as the @racket[Integer] check from
above are performant. However, contracts in general can
have a non-trivial performance impact, especially with the use of first-class
functions or other higher-order data such as vectors.

Note that no contract overhead is ever incurred for uses of typed
values from another Deep-typed module.


@subsection{Shallow Types: Sound Types, Low-Cost Interactions}

Changing the module language of the @racket[_client] program
from @racketmodname[typed/racket] to @racketmodname[typed/racket/shallow]
changes the way in which typed-untyped interactions are protected.
Instead of contracts, Typed Racket uses shape checks to enforce
these Shallow types.

With Shallow types, the @racket[_client] program from above still detects an
error when an untyped function returns a string instead of an integer:

@examples[#:label #f #:eval shallow-eval
(module client typed/racket/shallow

  (require/typed 'increment [increment (-> Integer Integer)])

  (increment 5))

(eval:error (require 'client))
]

The compiled @racket[_client] module has two shape checks in total:

@itemlist[#:style 'ordered
  @item{
    A shape check at the @racket[require/typed] boundary confirms that
    @racket[increment] is a function that expects one argument.
  }

  @item{
    A shape check after the call @racket[(increment 5)] looks for an integer.
    This check fails.
  }
]

Such checks work together within one typed module to enforce the assumptions that
it makes about untyped code.

In general, a shape check ensures that a value matches the top-level constructor
of a type.
Shape checks are always yes-or-no predicates (unlike contracts, which may wrap a
value) and typically run in constant time.
Because they ensure the validity of type constructors, shape checks allow Typed
Racket to safely optimize some programs---though not to the same extent as Deep
types.

@bold{Important caveats}: (1) The number of shape checks in a module grows in
proportion to its size. For example, every function call in Shallow-typed code
gets checked---unless Typed Racket is certain that it can trust the function.
Shallow types are therefore a poor choice for large, computationally-heavy
modules.
(2) Shallow types are only enforced in their immediate, local context.
For example, if typed code were to cast @racket[increment] to expect a string,
then the function could be called without an error.


@subsection{Optional Types: It's Just Racket}

A third option for the @racket[_client] program is to use Optional types, which
are provided by the language @racketmodname[typed/racket/optional]:

@examples[#:label #f #:eval optional-eval
(module client typed/racket/optional

  (require/typed 'increment [increment (-> Integer Integer)])

  (increment 5))
]

Optional types do not ensure safe typed-untyped interactions.
In fact, they do nothing to check types at run-time.
A call to the increment function does not raise an error:

@examples[#:label #f #:eval the-eval (require 'client)]

Optional types cannot detect incorrect type assumptions
and therefore enable zero type-driven optimizations.
But, they also add no costs to slow a program down.
In general, the behavior of an Optionally-typed program is the same as that of
a Racket program that completely ignores type annotations.


@subsection{When to Use Deep, Shallow, or Optional?}

@itemlist[
  @item{
    @emph{Deep} types maximize the benefits of static checking
    and type-driven optimizations.
    Use them for tightly-connected groups of typed modules.
    Avoid them when untyped, higher-order values frequently
    cross boundaries into typed code. Expensive boundary types
    include @racket[Vectorof], @racket[->], and @racket[Object].
  }
  @item{
    @emph{Shallow} types are best for small typed modules that frequently
    interact with untyped code.
    This is because Shallow shape checks run quickly: constant-time for
    most types, and linear time (in the size of the type, not the value)
    for a few exceptions such as @racket[U] and @racket[case->].
    Avoid Shallow types in large typed modules that frequently call functions
    or access data structures because these operations may incur shape checks
    and their net cost may be significant.
  }
  @item{
    @emph{Optional} types enable the typechecker and nothing else. Use them when
    you do not want types enforced at run-time.
  }
]



@close-eval[the-eval]
@close-eval[shallow-eval]
@close-eval[optional-eval]

