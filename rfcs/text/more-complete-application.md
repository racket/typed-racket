- Feature Name: more-complete-application
- Start Date: 30 July 2018
- RFC PR: (leave this empty)
- Feature Commit(s): (leave this empty)

# Summary

When a function has a `case->` type, calculating the result
type for a given set of well-typed arguments should
incorporate the range from each applicable `->` in the
`case->`.

# Motivation

This is a sound and natural extension of how Typed Racket
currently handles function types and can greatly simplify
some types (see the numeric tower, which has an explosion of
cases, one arrow for each supported use case), making them
simpler for developers to maintain and users to understand.

# Guide-level explanation

Some functions use a `case->` type, which indicates that
multiple simpler function types work together to describe
the function's behavior.

For example, a simple addition function designed to add
integers with the same sign could be written as follows:

```
(: plus (case->
         [Nonnegative-Integer Nonnegative-Integer -> Nonnegative-Integer]
         [Nonpositive-Integer Nonpositive-Integer -> Nonpositive-Integer]))
(define (plus x y)
  (+ x y))
```

When calculating the resulting type for specific use cases, each applicable
`->` type in the `case->` contributes to determining the result type:


```
(ann (plus 42 42) Nonnegative-Integer) ;; first arrow
(ann (plus -42 -42) Nonpositive-Integer) ;; second arrow
(ann (plus 0 0) Zero) ;; first _and_ second arrow!
```

In other words, the first two examples correspond to the
first and second arrows respectively, but the third example
fits both descriptions, and so the resulting type is the
intersection of both result types, i.e. the type which
describes values that are both nonnegative and nonpositive
integers (namely `Zero`).

# Reference-level explanation

At each function application site for a monomorphic `case->`
function, the resulting type is the intersection of the
results for each arrow whose domain covers the argument
types.

This is sound because a function cannot be assigned a
`case->` type unless its body type checks at each `->` type
in the `case->`.

# Drawbacks and Alternatives
[drawbacks]: #drawbacks

I can't think of any reasons to not adopt this change in the
long run; it is strictly a sound and more complete handling
of function application.

In the short term, because we're now more completely
reasoning about some `case->` types which explicitly
contained a very verbose specification (e.g. `+`) there will
be some slowdowns (i.e. instead of stopping at the first
applicable `->`, we continue checking all the others which
costs a little bit of time).

# Prior art
[prior-art]: #prior-art

Although this is _more_ complete than Typed Racket's
function application, it is still not fully complete. For a
complete handling, we would likely need a much richer notion
of subtyping (à la [CDuce](http://www.cduce.org/),
i.e. semantic subtyping)... however this is not something we
could easily add since we use a syntactic (and not semantic)
notion of subtyping. Although there has been some work
("Empowering Union and Intersection Types with Integrated
Subtyping", Muehlboeck and Tate, OOPSLA 2018) discussing how
to increase the expressiveness of subtyping in syntactic
systems we may wish to examine at some point.


# Unresolved questions [unresolved]: #unresolved-questions

## Performance Impacts

Here are some quick numbers on how this change will
initially affect type checking performance times
from the `tr-performance` package (found [here]()):

```
Compile time ratio (old time / new time):
    schml-specify-rep: 0.99 (i.e. slower, 8.63s (σ 0.09) to 8.72s (σ 0.05))
    schml-interp-casts-help: 1.01 (i.e. faster, 20.05s (σ 0.24) to 19.77s (σ 0.01))
    parser: 0.95 (i.e. slower, 2.22s (σ 0.05) to 2.33s (σ 0.09))
    old-metrics: 0.96 (i.e. slower, 2.68s (σ 0.04) to 2.78s (σ 0.09))
    new-metrics: 0.98 (i.e. slower, 3.61s (σ 0.02) to 3.7s (σ 0.08))
    math-flonum: 0.98 (i.e. slower, 3.76s (σ 0.02) to 3.83s (σ 0.08))
    fsm: 0.94 (i.e. slower, 4.91s (σ 0.09) to 5.22s (σ 0.19))
    forth: 0.97 (i.e. slower, 4.87s (σ 0.18) to 5.01s (σ 0.01))
    dungeon: 0.95 (i.e. slower, 8.64s (σ 0.11) to 9.11s (σ 0.18))
    bernoulli: 0.95 (i.e. slower, 3.95s (σ 0.04) to 4.16s (σ 0.09))
    acquire: 0.96 (i.e. slower, 15.53s (σ 0.23) to 16.2s (σ 0.08))
```

And for the math library, compile time initially increases
from 2m28.281s to 2m40.492s. (Note that, because the math
library is frequently using numeric tower operations, it is
expected that initially it will take a hit from our
increased efforts in function application calculations).


The hope is that after these changes, we can go improve what
are now overly verbose types and reduce the costs once
again.  However, it is possible there will still be some
residual overhead for some function applications.
