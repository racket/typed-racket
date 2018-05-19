- Feature Name: prefab-structs
- Start Date: 2018-04-24
- RFC PR: (leave this empty)
- Feature Commit(s): (leave this empty)

# Summary

Typed Racket's handling of #:prefab structs is currently (as of April 2018) unsound.
Instead of treating them like standard structs where we can assume (and enforce)
that fields always contain values of a certain type (i.e. the current behabior), 
prefabs should more-or-less be treated exactly like tagged n-ary cons/mcons cells.

# Motivation

Prefabs are clearly useful and desirable (e.g. they're serializable) and our current
lack of sound support for them should be addressed.

# Guide-level explanation

An __immutable prefab__ is in essence a n-ary tuple whose fields can contain
any value. They are defined with the following syntax:

```racket
(struct point ([x : Integer] [y : Integer])
  #:prefab)
```

This defines a predicate and accessors which work for _any_ compatible
prefab (i.e. any prefab with the same [prefab key](https://docs.racket-lang.org/reference/structutils.html#%28def._%28%28quote._~23~25kernel%29._prefab-struct-key%29%29)), and a constructor which expects two `Integer`s.

Examples:

```racket
(point? (point 1 2)) ; => #t
(ann (point 1 2) point)
(ann (point 1 2) (Prefab point Integer Integer))
(point "1" "2") ; => fails w/ type error "Expected Integer, Given String"
(point? #s(point "1" "2")) ; => #t
(ann #s(point "1" "2") (Prefab point String String))
(point-x (point 1 2)) ; => 1
(point-x #s(point "1" "2")) ; => "1"
(ann (lambda (x) 
       (if (point? x)
           ;; We don't know what the types for the fields
           ;; of `x` are; they could be anything.
           (ann x (Prefab point Any Any))
           42)))
     (-> Any Any))
```

A __mutable prefab__ (i.e. a prefab where each field is mutable) is 
defined by adding the keyword `#:mutable`:

```racket
(struct initials ([first : Symbol] [last : Symbol])
  #:prefab
  #:mutable)
```

and is the same as an immutable prefab except that when the
field types for the prefab are unknown, the type system will allow
those fields ty be read from but not written to (i.e. in the same 
way after testing `box?` tells the type system only that there is 
a box we can read from but not what kind of values can be written).

# Reference-level explanation

## Prefab Top Types

`(PrefabTop key field-count)` is the top type for the prefab described by the
key `key` which has `field-count` fields. For immutable prefab structs, 
this type is equivalent (in fact short hand for) the type 
`(Prefab key Any ...)` (where there are `field-count` occurrences of `Any` ). 
For mutable prefab structs, a `PrefabTop` type can be used for field reads 
(i.e. always producing a value of type `Any`) but cannot be used 
for field writes (since the type of the invariant field is not known).


## Immutable Prefabs

This __immutable prefab defininition__:

```racket
(struct point ([x : Integer] [y : Integer])
  #:prefab)
```

defines types/constructors/accessors for what can be thought of as a
 _named tuple_ type, producing the following type definition:

`(define-type point (Prefab point Integer Integer))` (_Note: the inner usage of `point`--- the prefab key in the type---is implicitly quoted_).

and the following functions:

- `point` of type `(-> Integer Integer point)`

- `point?` of type `(-> Any Boolean : (Prefab point Any Any))` 
(_NOTE: the type has `Any` not `Integer` in the field! This is because
        a prefab could have been created anywhere with any value in
        its fields_)

- `point-x` of type `(All (X) (-> (Prefab point X   Any) X))`

- `point-y` of type `(All (Y) (-> (Prefab point Any Y)   Y))`

Other things to know:

- `(make-predicate point)` will produce a function with type 
  `(-> Any Boolean : point)` which will inspect the fields to ensure
  they are `Integer`s.

- Prefab structures should be protected with a contract combinator
  almost identical to `struct/c`, but which instead only requires
  the prefab key and the field contracts. E.g., the type
  `(Prefab point Integer Integer)` should be contracted with
  a contract `(prefab/c 'point exact-integer? exact-integer?)` which
  accepts prefabs with key `'point` and two fields, both of which
  are exact integers. We believe this new combinator is necessary
  because `struct/c` requires actual struct/bindings identifiers
  and those are not necessary for prefabs (since they already
  exist and anyone can create them). _NOTE: This `prefab/c` combinator
  does not currently exist. An initial fix would likely include
  an implementation that may someday be added to `racket/contract`
  if that makes sense._
  

## Mutable Prefabs

This __mutable prefab defininition__:

```racket
(struct initials ([first : Symbol] [last : Symbol])
  #:prefab
  #:mutable)
```

defines types/constructors/accessors for what can be thought of as 
a _named heterogeneous vector_ type, producing the following type definition:

`(define-type initials (Prefab (initials #(0 1)) Symbol Symbol))` (_Note: 
the prefab key  `(initials #(0 1))` is implicitly quoted_).

and also produces the following definitions:

- `initials` with type `(-> Symbols Symbols initials)`

- `initials?` with type `(-> Any Boolean : (PrefabTop initials 2))`

- `initials-first` with type `(All (F L) (-> (Prefab initials F L) F))` _and_
type `(-> (PrefabTop initials) Any)` (i.e. a `case->`/intersection)

- `initials-last` with type `(All (F L) (-> (Prefab initials F L) L))` _and_
type `(-> (PrefabTop initials) Any)` (i.e. a `case->`/intersection)

- `set-initials-first!` with type `(All (F L) (-> (Prefab initials F L) F Void))`

- `set-initials-last!` with type `(All (F L) (-> (Prefab initials F L) L Void))`


# Prior art

Support for prefabs exists already, but has soundness problems. We
discuss the current details below.

## Current (i.e. Racket v6.12) prefab struct behavior in Typed Racket

As of Racket v6.12, immutable and mutable prefabs are supported 
as follows:

### Racket v6.12 Immutable Prefabs


```racket
(struct point ([x : Integer] [y : Integer])
  #:prefab)
```

defines types/constructors/accessors for a prefab struct, 
producing the following type definition:

`(define-type point (Prefab point Integer Integer))`

and the following functions:

- `point` of type `(-> Integer Integer point)`

- `point?` of type `(-> Any Boolean : point)` _NOTE: this is unsound since
  the fields could contain anything_

- `point-x` of type `(-> point Integer)` (this is sound but too limited, i.e.
it won't work for `point`'s with different field types (and it should))

- `point-y` of type `(-> point Integer)` (same issue as `point-x`)

### Racket v6.12 Mutable Prefabs

```racket
(struct initials ([first : Symbol] [last : Symbol])
  #:prefab
  #:mutable)
```

defines types/constructors/accessors for a mutable prefab struct, 
producing the following type definition:

`(define-type initials (Prefab (initials #(0 1)) Symbol Symbol))` 

and the following function definitions:

- `initials` with type `(-> Symbols Symbols initials)`

- `initials?` with type `(-> Any Boolean : initials)` (_NOTE: this is unsound
since the fields can actually have values of any type in them_)

- `initials-first` with type `(-> initials Symbol)` (_NOTE: this is likely sound
but too limited like the current immutable field accessors_)

- `initials-last` with type `(-> initials Symbol)` (_NOTE: same as previous accessor_)

- `set-initials-first!` with type `(-> initials Symbol Void)` (_NOTE: also too limited_)

- `set-initials-last!` with type `(-> initials Symbol Void)` (_NOTE: also too limited_)

## Backwards Compatibility

- Any previous programs that relied on a prefab's predicate and occurrence typing 
to determine not only that a value was a prefab, but also that it's fields
had values of a certain type, will break. 

E.g. this program will break:

```racket
(ann (lambda (p) (if (point? p) (point-x p) 42))
     (-> Any Integer))
```

I.e. we know `p` is a `point` _prefab_ of some sort in the then branch,
but not that it's fields have integers in them. 

This program will still work:

```racket
(ann (lambda (p) (if (point? p) (point-x p) p))
     (-> (U point Integer) Integer))
```

because we knew that `p` had type `point` OR it had type `Integer`, 
so the test for `point?` producing `#true` indicates it was not 
an `Integer` and is indeed a `point` _prefab_ with Integers in both its fields.

- Changing the accessors to be polymorphic could break programs that work
today but where our current limited type inference would fail (e.g. mapping
a struct accessor over a list of structs). We could mitigate this
by using a `case->` type with the old type after the polymorphic type if
we think it's worth it.

E.g. today this program type checks:

```racket
(map point-x
     (list (point 1 2) (point 3 4)))
```

but will break if we only use the polymorphic type for accessors 
(since our limited inference cannot infer types for polymorphic 
arguments to polymorphic functions)

# Drawbacks and Alternatives

The primary drawbacks are the backwards incompatibilities detailed in
the previous section.

Alternative approaches include:

1. Do nothing (i.e. leave things the way they are). However, this seems
particularly problematic since one of the advantages of prefab
structures is that they can be reliably serialized, and our current
predicate types for prefabs are unsound which means it would be
extremely easy to write unsound, buggy code when reading in data
and testing if it is a particular prefab.

e.g., this program reads in some data, uses `point?`, and then
erroneously assumes that the fields of the prefab struct
have integers when they in fact do not:

```racket
#lang typed/racket

(struct point ([x : Integer] [y : Integer])
  #:prefab)

(let ([data : Any (read (open-input-string "#s(point 1.5 2.5)"))])
  (if (point? data)
      (ann (point-x data) Integer) ;; oh no! this isn't an integer!
      42))
```

2. We could use monomorphic accessors (or only be as polymorphic as
the user type declaration):

```racket
(ann point-x (-> point Integer))
```

This would be sound (if we fix predicates!), but would mean we could
not access the fields of many prefabs, e.g. this would not type check:

```racket
#lang typed/racket

(struct point ([x : Integer] [y : Integer])
  #:prefab)

(let ([data : Any (read (open-input-string "#s(point 1.5 2.5)"))])
  (if (point? data)
      (ann (point-x data) Integer) ;; type error! expected a point, 
                                   ;; got a (Prefab point Any Any)
      42))
```

which would be frustrating... i.e. how else are we going to get
out the `x` in that `point`?

3. We could assign accessors/mutators polymorphic _and_ monomorphic types
so that if type inference fails, the monomorphic variant might work:

```racket
(ann point-x (All (X Y)
               (case-> (-> (Prefab point X Y) X)
                       (-> point Integer))))
```

This might mean that programs such as the following still
typecheck as they used to:

```racket
(map point-x
     (list (point 1 2) (point 3 4)))
```

i.e. today in Typed Racket (version 6.12), our type inference is
weak and so if `point-x` had only the polymorphic arrow type,
it would fail to type check, but with the monomorphic variant
to fall back on it would still work (if the list is indeed
a list of points with integers in each field).

4. Typed Racket could try to change the generated predicate
for prefabs so that the current types _are_ correct. I.e., Typed
Racket could have its own custom version of `#:prefab`
structs which creates a predicate that not only checks that
a value is a prefab of the particular key/field-count, but 
also checks that each field has values of the correct type.
However, this would only work for _first-order
data in immutable prefabs_... so it would arguably be a complicated 
and inconsistent fix with numerous caveats.

# Unresolved questions

- How many programs in the wild will these changes break?
  (_NOTE: prefabs have been pretty buggy in Typed Racket so
  far, so it is probably the case that at least some users
  who might have used them decided not to. _) The only
  observed breakage (one case) so far is unsound usages of a
  prefab predicate to read serialized data from disk
  (i.e. the current predicate tells the type system what is
  in the fields when in fact we do not know what is in the
  fields because we have not tested it). Hopefully we can
  quickly address any such issues with fixes that work
  with previous and past versions. E.g., the following
  unsound program will be broken by the proposed
  changes:

```racket
#lang typed/racket

(struct serialized-point ([x : Integer] [y : Integer]) #:prefab)
(struct point ([x : Integer] [y : Integer]) #:transparent)

(: try-read-serialized-point (-> Any (U point #f)))
(define (try-read-serialized-point data)
  (cond
    [(serialized-point? data)
     (point (serialized-point-x data)
            (serialized-point-y data))]
    [else #f]))

(try-read-serialized-point
 (read (open-input-string "#s(serialized-point one two)")))
;; produces a point with symbols, yikes!
```

and can be fixed in a way that works with current Racket and
after the proposed fixes in this RFC as follows:

```
#lang typed/racket

(struct serialized-point ([x : Integer] [y : Integer]) #:prefab)
(struct point ([x : Integer] [y : Integer]) #:transparent)

(: try-read-serialized-point (-> Any (U point #f)))
(define (try-read-serialized-point data)
  (cond
    [(serialized-point? data)
     (let ([x : Any (serialized-point-x data)]
           [y : Any (serialized-point-y data)])
       (if (and (exact-integer? x)
                (exact-integer? y))
           (point x y)
           #f))]
    [else #f]))

(try-read-serialized-point
 (read (open-input-string "#s(serialized-point one two)")))
 ;; produces #f, yay!
```

i.e. we add explicit tests to the fields, along with `Any`
annotations for the fields to tell the type system to
forget about the alleged types for those fields.

For a simpler fix (but that will only work after the proposed
changes have been merged and will not work in previous
versions) `define-predicate` can be used:

```racket
#lang typed/racket

(struct serialized-point ([x : Integer] [y : Integer]) #:prefab)
(struct point ([x : Integer] [y : Integer]) #:transparent)

(define-predicate valid-point? serialized-point)

(: try-read-serialized-point (-> Any (U point #f)))
(define (try-read-serialized-point data)
  (cond
    [(valid-point? data)
     (point (serialized-point-x data)
            (serialized-point-y data))]
    [else #f]))

(try-read-serialized-point
 (read (open-input-string "#s(serialized-point one two)")))
```


