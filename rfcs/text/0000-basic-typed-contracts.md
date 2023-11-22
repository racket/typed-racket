- Feature Name: basic-typed-contracts
- Start Date: 2019-06-30
- RFC PR: (leave this empty)
- Feature Commit(s): (leave this empty)

# Summary

Give TR typed analogues of the `racket/contract` module. This needs a
new type constructor, `Contract`, to support higher-order
contracts. And adding a `FlatContract` type constructor helps e.g. to
give a more precise type to combinators like `not/c`.

(The MS thesis related to this work is on my personal page at Tufts
<https://www.eecs.tufts.edu/~blachanc/ms-thesis.pdf>. I don't yet have
an arXiv link because I need to get my Scribble build working enough
to at least get the intermediate TeX out.

Any mention of "the current implementation" refers to the pull request
at <https://github.com/racket/typed-racket/pull/420>.)

# Motivation

Racket programmers use the contract forms provided by
`racket/contract` but there currently isn't a clear path for Typed
Racket programmers to do the same. Without those forms, enforcing
invariants outside of the type system's sweet spot is harder, and
migrating contract-using Racket programs to Typed Racket is harder.

The current implementation supports most of the data structure
contracts in the contract documentation, and it also supports most of
`->i`, `->*`, and `contract-out`. (But the `struct/c` form isn't yet
supported, and things like the `struct` *contract-out-item* may need
special support from a TR version of `contract-out`).

# Guide-level explanation

## Basics/contracts over first-order types

(Assumes a passing knowledge of Racket's contract library and TR's
propositions. In typed contracts, I write `->/c` for Racket's `->`
contract combinator.)

Racket's contract system monitors how values flow through
contracts. Using those same contracts in Typed Racket means we'll need
to give typed values to the contract system, and so the type system
will have to ensure that those values are used appropriately.

We can use typed functions as contracts. To properly use a typed
function, the type system has to ensure that the contract is only ever
used to monitor values that agree with the function's type. Being a
predicate on `Integer`s, we can use `even?` to monitor integers:

`(contract even? -6 'pos 'neg)`
`(contract even? 6 'pos 'neg)`

We can also combine typed functions using contract combinators like
`and/c`:

`(contract (and/c even? positive?) 10 'pos 'neg)`

When we combine contracts, we need to be careful that the combination
makes sense. But first we need to talk about the types of the
contracts *being* combined. We've already seen that we need to make
sure a function is only used as a contract to monitor values of the
right type. But one idiom in Racket is to write contracts like `(and/c
exact-integer?  even?)`: Here, a (Racket) contract programmer uses the
`exact-integer?` test to guard that `even?` in their contract is only
applied to integers. Without the guard, `even?` might raise a runtime
error (its own contract violation) unrelated to the contract the
programmer wrote. To give types to these contracts, then, we need to
know that whatever comes out of the `exact-integer?` contract is safe
to flow into the `even?` contract. So in addition to needing to know
the types of values a contract can be used on, we also need to know
that what comes out of a contract is safe for some subsequent
contract, like in `and/c`. Typed Racket's propositions help us uncover
this last part.

The two parts of a contract's type explain what the contract can be
used on and what comes out of the contract. For `exact-integer?`, its
function type says it can be used on `Any` and that it has a positive
proposition for `Integer`, thus it has contract type `(Contract Any
Integer)`. The type of `even?` is `(-> Integer Boolean)`, with no
propositions, and so it has contract type `(Contract Integer
Integer)`. We say that the first part of a contract's type is its
*input type* and that the second part of a contract's type is its
*output type*. For example, the input type of `exact-integer?` is
`Any` and its output type is `Integer`.

Back to type checking the combined contract, `(and/c exact-integer?
even?)`. From the types above, we know that what comes out of
`exact-integer?` can safely go in to `even?`. And since everything
lines up, we take the the input type of the first contract type
(`Integer`) and the output type of the last contract (`Integer`) and
give the whole contract type `(Contract Any Integer)`.

---

As another example, this is a contract for lists of positive numbers:

    (listof positive?)

Since `positive?` has type `(-> Real Boolean : #:+ Positive-Real)`,
this list contract has type `(Contract (Listof Real) (Listof
Positive-Real))`.

The `listof` combinator itself has type `(-> (Contract a b) (Contract
(Listof a) (Listof b)))`. (Maybe we could give it a more specific type
with `case->` so that when applied to a `FlatContract`, `listof`
returns a `FlatContract`.)

## Contracts on higher-order types

If a function only consumes and produces positive reals, a Racket
programmer might express that information using the contract `(->/c
positive? positive?)`. In Typed Racket, that same information could be
expressed with the type `(-> Positive-Real Positive-Real)`. So
although that exact contract might not make sense in a Typed Racket
program, we still want to give it a type---the path from Racket to
Typed Racket isn't always smooth so we might be able to help the
Racket programmer if we give a meaningful type to that contract.

Starting with the function `positive?`, its type is `(-> Real
Boolean)` and it has a positive proposition for `Positive-Real`. So
based on what we saw in the previous section, it has the contract type
`(Contract Real Positive-Real)`.

The function contract combinator `->/c` wraps whatever function it is
applied to, ensuring that a caller provides arguments that satisfy the
domain contract and that the function returns a result that satisfies
the range contract; Values flow from the caller through the domain
contract into the function, and out of the function and through the
range contract.

This value flow is reflected in the type of a function contract. For
our `(->/c positive? positive?)` the type is

    (Contract (-> #|2|#Positive-Real #|3|#Real)
              (-> #|1|#Real #|4|#Positive-Real))

In the output type of the contract, here a function type, the domain
type is `Real` (marked `#|1|#`). This is because after applying the
contract to a function, calling the result with a value will apply the
domain contract (with input type `Real`) to that value. If the value
passes the domain contract, we know it is a `Positive-Real` by the
domain contract's output type, which is reflected in the input type's
domain contract (marked `#|2|#`). This means that a function we apply
this contract to has to accept `Positive-Real` values.

In the input type of the contract, the range of the type is `Real`
(marked `#|3|#`) because applying the contract to a function is only
allowed if that function returns a value that can flow through the
range contract (whose input type is `Real`). Finally, in the output
type of the contract the range type is `Positive-Real` (marked `#|4|#`)
because that's the output type of the range contract. This means that
if we apply this contract to a function, then the resuling function
returns at least a `Positive-Real` if it returns at all.

# Reference-level explanation

## Interaction with other features

As far as I can tell contracts are mostly self-contained. Some
exceptions:

  - Predicates are subtypes of the `FlatContract` type.

  - Values of `FlatContract` type are not functions (`FlatContract`
    currently corresponds to Racket's `flat-contract?` which is `#t`
    for e.g. numbers)

  - Applying a function contract (with the current type rule for
    contract application) combines the range type of the function
    contract's output type with the range type of the function that
    will be monitored. In the code this is called "pairwise
    intersection" and in the thesis this is the `comb` operator.

As far as subtyping goes, `(Contract S T)` and `(FlatContract S T)` is
contravariant in `S` and covariant in `T`. And a `FlatContract` type
is a subtype of a `Contract` type according to the same variance
conditions.

## How the feature would be implemented

The MS thesis linked up top gives type rules formalizing what I walked
through in the guide-level explanation. It also discuss some of the
implementation techniques used for macro-based combinators like `->i`.

Adding the *types* to TR is just like adding other types: define new
type constructors with the proper variance, define aliases
(e.g. `FlatContract` as a union type), extend the parser/pretty
printer, and add cases to the subtyping procedure.

## Corner cases

Because I haven't worked out how to compile a contract type to a
contract for typed/untyped interaction, typed code can't let untyped
code use a value of contract type. At the moment I believe the current
implementation raises an error (maybe an internal one?) when providing
a value of contract type because I haven't defined a way to compile
those types to contracts.

I haven't properly looked at how these contract types interact with
polymorphic types. I'll update the PR to reject polymorphic types that
appear in contracts and to reject functions like `null?` (which has a
polymorphic type) from being used like contracts. (This sounds tricky:
we want combinators like `listof` to have a function type where
possible, which currently relies on polymorphism, but we want to
reject attempts to construct a contract over polymorphic types. At any
rate, I'll try to reject this polymorphism corner case with a useful
error message.)

# Drawbacks and Alternatives

[drawbacks]: #drawbacks

I don't see drawbacks in adding typed contracts themselves to Typed
Racket; I see a potential drawback to the type system I've proposed.

The rule for contract application is complicated by the two-argument
`Contract` type, and it may not be worth the cognitive overhead. One
reason for the two type arguments is so that the type computed by the
`and/c` rule is not far from what you get from a sequence of contract
applications (i.e. the `and/c` rule is privileged, but with the two
type arguments it's not as privileged). But this may not be worth the
complexity.

Using a unary type constructor would remove the whole notion of input
and output types, which could make for an easier type rule for the
`and/c` combinator and for the other combinators, too.

# Prior art
[prior-art]: #prior-art

I haven't seen a language that combines typed, higher-order contracts
and subtyping---the combination seems to be unique to bringing
Racket's contracts to Typed Racket. The current design is written up
in the MS thesis linked up top.

The original higher-oder contract paper [ff-icfp2002] uses contract
types in a simply typed language, and that type system was also used
in some work around higher-oder assertions and Haskell
[hjl-flp2006]. The Haskell work supports polymorphism but doesn't have
subtyping.

- [ff-icfp2002] Robert Bruce Findler and Matthias Felleisen. Contracts
for Higher-Order Functions. ICFP 2002
- [hjl-flp2006] Ralf Hinze, Johan Jeuring, and Andres Löh. Typed
Contracts for Functional Programming. Functional and Logic
Programming, 2006.

# Unresolved questions
[unresolved]: #unresolved-questions

> What parts of the design do you expect to resolve through the RFC
> process before this gets merged?

Whether the two-argument `Contract` type constructor and the
corresponding contract application rule is too complex

> What parts of the design do you expect to resolve through the
> implementation of this feature before stabilization?

Giving better error messages, simplifying tricky parts of the
implementation (assuming that's in scope for "parts of the design";
e.g. recovering the dependency identifiers in a ->i contract from a
fully expanded program)

> What related issues do you consider out of scope for this RFC that
> could be addressed in the future independently of the solution that
> comes out of this RFC?

- Types for object contracts, unit contracts, parametric contracts
  (i.e. `racket/contract/parametric`)

- Typed/untyped interaction at contract types
