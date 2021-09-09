- Feature Name: Kind System
- Start Date: 9/27/202
- RFC PR: #1143
- Feature Commit(s): PR #1143

# Summary

Typed Racket currently conflates polymorphic with type constructors.  This RPC
aims to provide a kind system to separate those two concepts. Note that higher
kinds and type-level lambdas will be discussed in difference RFCs.

# Motivation

Due to lack of a kind system, the type checker appromimates type-level
abstractions and applications a la recursive and polymorphic types, which has
caused confusion and issues. For example, `Listof`, which is actually a type
constructor, has kind `*`, because it is bound to `(All (a) (Rec b (U Null
(Pairof a b)))`. User-define types also face the same problem:

```racket
(define-type (I a) a)
(define (afunc [arg : I]) : Void
    (void))
```

Ideally, the program should not type-check because `I` is a type constructor
with the kind `* => *`. However, it type-checks because the type checker
treats `I` as an alias to `(All (a) a)`.

# Guide-level explanation

## Type constructors

Typed Racket provides built-in type constructors, such as `Pairof`,
`Listof`. Users can also define their own type constructors using the form
`define-type`.

## Programs annotated with ill-kinded types will be rejected

Due to the conflation of type constructors and polymorphic types, previously a
binding supposedly for a type constructor could be used as a shorthand for a
polymorphic type as shown above.  With the new changes, that program will get
rejected and the type-checker will report `I` in `[arg : I]` is not a type.

## Checking productivity of recursive types

The type variable of a recursive type must appear in productive position,
i.e. arguments to application of productive type constructors. For example:

- (Rec x x) is unproductive.

- (Rec x (U x Integer)) is unproductive, because U is unproductive and x is the
  same the level of the enviroment when checking operands to U.

- (Rec x (U Null (Pairof x Integer))) is productive, because U is productive and x
  is at one-level lower than the enviroment when checking operands to Pairof.

- (Rec x (Pairof (U x Integer) Integer)) is productive.


# Reference-level explanation

## Changes to define-type

`define-type` still has two forms:

- (define-type new-t t) creates new-t, an alias to t at the right hand side.

- (define-type (new-t a ...) t) creates an n-ary type constructor named `new-t`,
  whereas previously `new-t` was an alias to the polymorphic type `(All (a ...)
  t))`


## Print the kind of a type at the REPL.

`:kind`, is added for interactive uses at the Typed Racket REPL.  It prints the
 kind of a type-level expression. For example, `(:kind Integer)` will print `*`
 and `(:kind Listof)` will print `(-> * *)`.

## a valid syntax of type-level application

In `(tc t ...)`, `tc` must be a type constructor and the number of `t`s must at
least match the number of mandatory parameters of `tc` if `*...` is specified in
the kind of `tc`. Otherwise, the two numbers have to be an exact match.


## from container types to type constructors

All the base types now have kind *. Except the ones suffixed with `Top`, all the
other types listed in the subsection `container types` are type constructors
now. In other words, if we use :kind to inspect those types, the result will be
in the form of `(-> * ... *)` or `(-o * ... *)`. `->` and `-o` denotes a
productive and a unproductive type constructor respectively.  For example,
`Pairof` has kind `(-> * * *)`, `Listof` has kind `(-> * *)` and `U` has kind
`(-o * ... *)`. The status of a type constructor's productivity plays a critcal
role in checking if a recurive type is productive a not.


# Drawbacks and Alternatives
[drawbacks]: #drawbacks

## Drawbacks
1. adds complexity to the type checker.
2. adds backward incompatibility. See the next section.

## Backward Compatibility

1. some user-provided recursive type aliases that passed the previous
productivity check will not due to the improvements.

```racket
(define-type (D a) (B a))
(define-type (B a) (Listof a))
(define-type F (U (D F) (B F)))
```

The type `F` above is not productive, but the type checkers of past versions
fail to catch it.

2. as shown above, some programs abuses bindings supposedly for type
constructors as shorthands for polymporphic types and they type-check with in
previous verions of Typed Racket because of the special treatment of
polymorphic. However, with the new kind system, such a program will be
rejected. E.g,

```racket
(define-type (M a) (Listof a))
(: foo (-> M Void))
(define (foo m)
  (void))
```

3. Some code that uses nested polymorphic types as higher kinds will be also
rejected, because higher kinds is not considered in this RFC.

```racket
(define-type (M a) (All (b) (Pairof a b)))
(: foo (-> ((M Void) Void)) Void)
(define (foo m)
  (void))
```

One possible solution is to uncurry the above type constructors.

```racket
(define-type (M a b) (Pairof a b))
(: foo (-> (M Void Void)) Void)
(define (foo m)
  (void))
```

However, it is not always the case that uncurrying will work.


# Prior art
[prior-art]: #prior-art

As of Racket 8.3, Typed Racket doesn't have a kind system.