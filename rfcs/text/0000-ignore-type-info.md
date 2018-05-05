- Feature Name: ignore-type-info
- Start Date: 2018-5-1
- RFC PR: (leave this empty)
- Feature Commit(s): (leave this empty)

# Summary

Add a syntax property that causes typechecking to ignore the synthesized and
expected types and instead produce the type `Any` along with trivial
propositions and objects. The expression must typecheck at the type `Any`.

The property is named `typed-racket:ignore-type-information`. 

# Motivation

This provides built-in support for a pattern where the programmer
wants write a test that is always true (or false) but the typechecker
can't tell that. Currently, it's easy to do this as follows:

```
(if (> (random 5) 6) 'never 'always)
```

However, it's problematic to rely on the type checker never
understanding things in the future, and also we'd like to enable the
bytecode compiler to optimize the test. 

This was developed specifically to support this pattern in a revised
version of the keyword argument protocol currently under development
by @mflatt.

Note that simply using `ann` is not possible here, both because this property is intended to be used from places (such as `racket/private/kw`) that do not depend on Typed Racket, and because `ann` does not have the needed behavior with respect to propositions.

# Guide-level explanation

Any expression with the shape `(#%expression sub)` that has a true
value for the syntax property `typed-racket:ignore-type-information`
will have the type `Any`, and the type checker won't learn anything
about the expression for use in refining other types.

The expression `sub` must still type check, but can have any
single-valued type.

# Reference-level explanation

Forms of the `(#%expression sub)` that has a true value for the syntax
property `typed-racket:ignore-type-information` type check the
expression `sub` with expected type `Any`. The resulting `tc-result`
has type `Any` along with `true-prop` for both propositions and
`empty-obj` for the object.



# Drawbacks and Alternatives
[drawbacks]: #drawbacks

#### Drawbacks

Adds complexity to the type checker.

#### Alternative: use expected type

If there's an expected type when checking `(#%expression sub)` with
the relevant property, we could instead use the outer expected as both
the expected type when checking `sub` as well as the result of type 
checking the entire expression.

This allows stricly more programs to type check. If we have a macro
`mark` that added the relevant property, then this would type
check under this alternative:

```
(ann (lambda () (mark (#%expression "hi"))) (-> String))
```

whereas with the proposed design, it's an error since the body has
type `Any`.  

In this alternative, we woul check `"hi"` with the expected type `String`, and the result of checking the form `(mark (#%expression "hi")))` would be `String : Top|Top`. 
This alternative design would also satisfy the specific
use case outlined above, because there is no expected type for the test position of an `if` expression, and because the propositions are explicitly erased.

However, I propose the initial design as it is more conservative and
no use cases have been identified for this alternative.

# Implementation

This Pull Request also contains an initial implementation of the
feature, although lacking in tests and documentation.
