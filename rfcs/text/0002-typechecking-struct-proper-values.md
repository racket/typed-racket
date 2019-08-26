- Feature Name: type-checking-struct-property-values
- Start Date: 7/8/2019
- RFC PR: #803
- Feature Commit(s): PR #804

# Summary

This RFC aims to provide initial support for typechecking struct property values.

# Motivation
By adding types for struct properties, we further complete the type annotations
for struct types.

# Guide-level explanation
To type-annotate a struct type property, we use the `Struct-Property` form,
which takes only one argument to describe the expected type of that property
value. Currently, users cannot use this form as the type constructor to define a
struct property type. Here is an example of a type annotation for the
`prop:input-port` struct property:

```racket
(: prop:input-port (Struct-Property (U Input-Port Nonnegative-Integer)))
```

This specifies the value of a struct's `prop:input-port` property to be either
an input port or a nonngeative integer. Many struct properties, however, are
intended to contain a function when takes the associated struct instance as an
argument. To construct appropriate type for such a function, we use `Self` to
specify which argument must be the associated struct instance. For example, here
is a type annotation for `prop:custom-write` struct property:

```racket
(: prop:custom-write (Struct-Property (-> Self Port Boolean Void)))
```

This specifies that the value of a struct's `prop:custom-write` property should have
the function type `(-> Self Port Boolean Void)`, i.e. a function will be given
the associated struct instance as its first argument. Note that `Self` is only a
valid type within a `Struct-Property` declaration. When defining a struct with a
struct property whose type features `Self`, we instead use the name of struct
currently being defined wherever `Self` appeared in the struct property's type
declaration. For example, here is a struct `Point` which has the
`prop:custom-write` struct property mentioned above:

```racket

(struct Point ([x : number][y : number])
    #:property prop:custom-write
        (lambda ([self : Point]
                 [p : Port]
                 [mode : Boolean])
             (print (cons (Point-x self) (Point-y self) p))))

```

In addition, some struct properties requires a function to take not only the
associated struct instance, but also other instance created by the same struct
constructor. To construct appropriate type for such a function, we use `Imp` to
specify the implementor type. For example, here is a type annotation for
a made-up `prop:equal` struct property (inspired by `prop:equal+hash`):

```racket
(: prop:equal (Struct-Property (-> Self Imp (-> Any Any Boolean) Boolean)))
```

This specifies that the a value of a struct's `prop:equal` property should be a
function, whose second argument will be an instance of the same implementator
type but not the associated struct instance. Note that `Imp` is only a valid
type within `Struct-Property` declaration. When defining a struct with a struct
property whose type features `Imp`, we instead use the name of struct currently
being defined wherever `Imp` appeared in the struct property's type
declaration. But for polymorphic struct types, we introduce new type variables
and use them with type constructors for `Imp`. For example, here is a
polymorphic struct `Point` which has the `prop:equal` struct property
mentioned above:

```racket
(struct (T V) Point ([x : T][y : V])
  #:property prop:equal
  (lambda (A B)
    ([self : (Point T V)]
     [other : (Point A B)]
     [conv : (-> Any Any Boolean)])
    #f))
```


# Reference-level explanation
## Struct Type Property Types
`(Struct-Property ty)` is a new structral type, where its argument can be any
valid type in Typed Racket. All built-in struct type properties are
type-annotated.

`Self` and `Imp` are added as unique free variables to specify the instance
value which a property is extracted from and the implementor type. They will
only appear in `(Struct-Property)`

## Type-checking Struct Property Values In Struct Definitions
1. A new field `props` is added to the structure of `struct type` used in the
   type checker
2. After a `struct` is parsed, its attached properties and their values are retained
   for the following type checking.
3. During type checking, `Self` will be substituted with the `struct type` that
   the property is attached to. `Imp` will be substituted with the `struct
   type`. If it is polymorphic, its type variables will be replaced with fresh ones.



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
`prop:custom-write` is just `Struct-Type-Property`, which contains no specific
type information it expects. Also, provided struct properties within struct
definitions are not type checked in versions <= 7.2

# Unresolved questions
[unresolved]: #unresolved-questions
