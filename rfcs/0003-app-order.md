- Feature Name: Use left-to-right application order with occurrence typing
- Start Date: 2018-05-02
- RFC PR: (leave this empty)
- Feature Commit(s): (leave this empty)

# Summary

When typechecking a function application `(f e_0 ... e_n)`:

- use the unconditional props from `f` to type-check `e_0`
- use the unconditional props from `e_0` to type-check `e_1`
- ... and so on

# Motivation

The Typed Racket type checker is flow-sensitive in some ways.

- for an if-statement `(if e_0 e_1 e_2)`, the result of type-checking `e_0`
  is used to type check `e_1` and `e_2` with refined type environments
- for a sequence `(begin e_0 e_1)`, the result of type-checking `e_0`
  is used to refine the environment for type-checking `e_1`

This flow sensitivity is sound because it matches Racket's order of evaluation.
For example in `(begin e_0 e_1)`, the expression `e_1` is only evaluated after
the expression `e_0`.

Typed Racket is not currently flow-sensitive for function applications, even
though Racket guarantees left-to-right evaluation (Racket Reference, Sec 3.7).
For example:

```
#lang typed/racket

;; This expression type-checks, because TR is flow-sensitive for `begin`
(lambda ((x : Any))
  (begin
    (assert x integer?)
    (+ x x)))

;; This similar expression does NOT type-check, because TR doesn't know the
;;  second `x` has type `integer?`
(lambda ((x : Any))
  (+ (begin (assert x integer?) x)
     x))
```


# Guide-level explanation

The Typed Racket type checker includes some basic flow sensitivity:

- when type-checking a sequence of expressions, e.g. `(begin e_0 e_n)`, TR
  uses the result of checking `e_0` to type-check `e_1`
- when type-checking a function application, e.g. `(e_0 e_1)`, TR uses the result
  of type-checking the function expression `e_0` to type-check the argument
  expression `e_1`

<!-- Explain the proposal as if it was already included in the language and you were -->
<!-- teaching it to another Typed Racket programmer. That generally means: -->
<!--  -->
<!-- - Introducing new named concepts. -->
<!-- - Explaining the feature largely in terms of examples. -->
<!-- - Explaining how Typed Racket programmers should *think* about the feature. -->
<!--  -->
<!-- For implementation-oriented RFCs (e.g. for type checker internals), focus on how -->
<!-- type system contributors should think about the change, and give examples of its -->
<!-- concrete impact. -->
<!--  -->
<!-- # Reference-level explanation -->
<!--  -->
<!-- Explain the design in sufficient detail that: -->
<!--  -->
<!-- - Its interaction with other features is clear. -->
<!-- - It is reasonably clear how the feature would be implemented. -->
<!-- - Corner cases are dissected by example. -->
<!--  -->
<!-- Return to the examples given in the previous section, and explain more fully how -->
<!-- the detailed proposal makes those examples work. -->
<!--  -->
<!-- # Drawbacks and Alternatives -->
<!-- [drawbacks]: #drawbacks -->
<!--  -->
<!-- Why should we *not* do this? Could we do something else instead? -->
<!--  -->
<!-- # Prior art -->
<!-- [prior-art]: #prior-art -->
<!--  -->
<!-- Discuss prior art, both the good and the bad, in relation to this proposal. -->
<!-- A few examples of what this can include are: -->
<!--  -->
<!-- - Does this feature exist in other programming languages and what experience have their community had? -->
<!-- - Papers: Are there published papers, books, great blog posts, etc that discuss this? Be _specific_! -->
<!--  -->
<!--  -->
<!-- # Unresolved questions -->
<!-- [unresolved]: #unresolved-questions -->
<!--  -->
<!-- - What parts of the design do you expect to resolve through the RFC process before this gets merged? -->
<!-- - What parts of the design do you expect to resolve through the implementation of this feature before stabilization? -->
<!-- - What related issues do you consider out of scope for this RFC that could be addressed in the future independently of the solution that comes out of this RFC? -->
