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
    #:prop prop:custom-write
        (lambda ([self : Point]
                 [p : Port]
                 [mode : Boolean])
             (print (cons (Point-x self) (Point-y self) p))))

```

Note that using `Self` in value expressions for properties like
`prop:custom-write` fails the typechecker.


# Reference-level explanation


# Drawbacks and Alternatives
[drawbacks]: #drawbacks


# Prior art
[prior-art]: #prior-art


# Unresolved questions
[unresolved]: #unresolved-questions
