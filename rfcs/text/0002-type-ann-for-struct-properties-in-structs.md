- Feature Name: type-annotations-for-struct-properties-in-structs
- Start Date: TODO
- RFC PR: (leave this empty)
- Feature Commit(s): (leave this empty)

# Summary

Typecheck a value expression for a struct property in a struct definition with
expected type information in the property.

# Motivation
By adding types for struct properties, we further complete the type annotations
for struct types.

# Guide-level explanation
struct type properties are type-annotated with `Struct-Property`, where the only
argument is the expected type of property value expressions. Here is an example
of `prop:input-port`:

```racket
(: prop:input-port (Struct-Property (U Input-Port Nonnegative-Integer)))
```

This specifies the value in structs for `prop:input-port` to be either an input
port or a nonngeative integer. Usually a value for a struct property is a
function which takes the struct instance that the property would be extracted
from as the first argument. To construct such struct property, we use `Self` to
specify type of expected instances in the type annotation. Here is an example of
`prop:custom-write`:

```racket
(: prop:custom-write (Struct-Property (-> Self Port Boolean Void)))
```

This specifies the value in structs for `custom-write` to have the function type
`(-> Self Port Boolean Void)` where `Self` must be the struct instance that the
accessor to `custom-write` is invoked with. In the definition of a struct where
we attach properties that will operate on the struct instances, we specify the
type of the value using the struct type to match `Self` in the type annotation
of the struct property. Here is an example of using `prop:custom-write` in the
definition of the struct `Point` :

```racket

(struct Point ([x : number][y : number])
    #:property prop:custom-write
        (lambda ([self : Point]
                 [p : Port]
                 [mode : Boolean])
             (print (cons (Point-x self) (Point-y self) p))))

```

Note that using `Self` in value expressions for properties like
`prop:custom-write` fails the typechecker.


# Reference-level explanation
## Struct Type Property Types
`(Struct-Property ty)` is a new structral type, where its argument can be any
valid type in Typed Racket. User aren't allowed to use this form as the type
constructor to define a struct property type. All built-in struct type
properties are type-annotated.

In addition, `Self` is added as a singleton type to specify the instance value
which a property is extracted from. `Self` will only appear in `(Struct-Property)`

## Type-checking Struct Property Values In Struct Definitions
1. A new field `props` is added to the structure of `struct type` used in the
   type checker
2. After a `struct` is parsed, its attached properties and their values are retained
   for the following type checking.
3. During type checking, `Self` will be substituted with the `struct type` that
   the property is attached to.


# Drawbacks and Alternatives
[drawbacks]: #drawbacks

## Drawbacks
Adds complexity to the type checker.

## Backward Compatibility
The old `Struct-Type-Property` becomes an alias to `(Struct-Property Any)`

# Prior art
[prior-art]: #prior-art

As of Racket 7.2, most struct properties doesn't work in Typed Racket. Type
information of struct type properties is incomplete. For example, the type of
`prop:custom-write` is just `Struct-Type-Property`, which contains no specific type
information it expects. Also, there is no type-checking property values against
properties.


# Unresolved questions
[unresolved]: #unresolved-questions
