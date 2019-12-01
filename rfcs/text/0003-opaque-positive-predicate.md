- Feature Name: opaque-positive-predicate
- Start Date: 2019-11-30
- RFC PR: (leave this empty)
- Feature Commit(s): (leave this empty)

# Summary

The predicate in an `Opaque` type does not correspond to an exact decision yes/no, only a positive yes/unknown. This RFC defines the meaning of `Opaque` as the set of values the predicate has returned true for _at any point_ without excluding values the predicate has returned false for. It also provides `define-positive-predicate`, which defines positive yes/unknown predicates for types.

# Motivation

Currently `Opaque` and `#:opaque` are unsound because the result of the predicate on a value can change due to mutation (https://github.com/racket/typed-racket/issues/457). The unsoundness comes from a combination of two things:
  1. The predicate `foo?` is treated as an exact decision yes/no,
     in other words `foo? : (-> Any Boolean : Foo)`.
  2. The result of `(foo? x)` can change due to mutable state from `x`
     or even `foo?`.

This RFC fixes this unsoundness by changing (1) to a positive predicate, with type `(-> Any Boolean : #:+ Foo)`.

With a positive predicate, the type system still supports cases like:
```racket
; x : Any
(cond [(foo? x) (use-foo x)]
      [else     #f])
```

However it no longer supports cases like:
```racket
; x : (U Foo String)
(cond [(foo? x) #f]
      [else     (use-string x)])
```
Because `(foo? x)` returning false no longer rules out the type `Foo`. A program like this would need `(string?x)` determine `String`.

# Guide-level explanation

## `require/typed` `#:opaque`

```racket
(require/typed m rt-clause ...)

      rt-clause = ...other-rt-clause...
                | [#:opaque t pred]
```
The `[#:opaque t pred]` case defines a new type `t` and imports `pred` from module `m`. `pred` is a predicate for the type `t`. The type is defined as those values for which `pred` has produced `#t`. `pred` must have type `(Any -> Boolean : #:+ t)`. Opaque types must be required lexically before they are used.

Examples:
```racket
> (require/typed racket/base
                 [#:opaque Evt evt?]
                 [alarm-evt (Real -> Evt)]
                 [sync (Evt -> Any)])
> evt?
- : (-> Any Boolean : #:+ Evt)
#<procedure:evt?>
> (sync (alarm-evt (+ 100 (current-inexact-milliseconds))))
- : Any
#<alarm-evt>
```

## `Opaque`

```racket
(Opaque pred)
```
A type constructed using the `#:opaque` clause of `require/typed`. Defined as those values for which `pred` has produced `#t`.

Examples:
```racket
> (require/typed racket/base
                 [#:opaque Evt evt?])
> (:type Evt)
(Opaque evt?)
```

## `define-positive-predicate`

```racket
(make-positive-predicate t)
```
Evaluates to a predicate for the type `t`, with the type `(Any -> Boolean : #:+ t)`. When the predicate returns true on a value, that value is known to have type `t`. However, when the predicate returns false, the type is unchanged.

`t` may not contain function types, or types that may refer to mutable data such as `(Vectorof Integer)`. `make-positive-predicate` works with `Opaque` types such that `(make-positive-predicate (Opaque pred))` has the same runtime behavior as `pred`.

```racket
> (make-positive-predicate Integer)
- : (-> Any Boolean : #:+ Integer)
#<procedure:exact-integer?>
> ((make-positive-predicate Integer) 12)
- : Boolean
#t
```

```racket
(define-positive-predicate name t)
```
Equivalent to `(define name (make-positive-predicate t))`, defining `name` with the type `(Any -> Boolean : #:+ t)`. When `name` returns true on a value, that value is known to have type `t`. However, when `name` returns false, the type is unchanged.

```racket
> (define-positive-predicate listof-integer? (Listof Integer))
> listof-integer?
- : (-> Any Boolean : #:+ (Listof Integer))
#<procedure:listof-integer?>
> (listof-integer? (list 3 5 8))
- : Boolean
#t
```

# Reference-level explanation

## `require/typed` `#:opaque`

```racket
(require/typed m rt-clause ...)

      rt-clause = ...other-rt-clause...
                | [#:opaque t pred]
```
An `#:opaque` clause within `require/typed` expands to `require/opaque-type` from `typed-racket/base-env/prims-contract.rkt`:
```racket
(require/opaque-type t pred m maybe-unsafe maybe-name-exists)

       maybe-unsafe =
                    | unsafe-kw

  maybe-name-exists =
                    | #:name-exists
```
It imports `pred` from `m`, guarded with contract `(any-wrap-warning/c . c-> . boolean?)` unless `unsafe-kw` is provided. It defines `t` as a type alias for `(Opaque pred)`, and gives `pred` the type `(-> Any Boolean : #:+ (Opaque pred))`.

## `Opaque`

```racket
(Opaque pred)
```
A type constructed using the `#:opaque` clause of `require/typed`. Defined as those values for which `pred` has produced `#t`. Note that `pred` returning `#f` on a value does _not_ exclude the value from having type `(Opaque pred)`, since `pred` might return `#t` at a different time.

## `define-positive-predicate`

```racket
(make-positive-predicate t)
```
Evaluates to a predicate for the type `t`, with the type `(Any -> Boolean : #:+ t)`. When the predicate returns true on a value, that value is known to have type `t`. However, when the predicate returns false, the type is unchanged.

`t` may not contain function types, or types that may refer to mutable data such as `(Vectorof Integer)`. `make-positive-predicate` works with `Opaque` types such that `(make-positive-predicate (Opaque pred))` has the same runtime behavior as `pred`.

```racket
(define-positive-predicate name t)
```
Equivalent to `(define name (make-positive-predicate t))`, defining `name` with the type `(Any -> Boolean : #:+ t)`. When `name` returns true on a value, that value is known to have type `t`. However, when `name` returns false, the type is unchanged.

## Interaction with define-predicate

Using `make-predicate` or `define-predicate` on types with `Opaque` in them produces a _warning_ for now. Later this warning should be changed to an error to fully close the unsoundness shown in Motivation and https://github.com/racket/typed-racket/issues/457. For example:
```racket
(require/typed racket/base
               [#:opaque Evt evt?])
(define-predicate evt?-v2 Evt)
```
produces a warning, while using `define-positive-predicate` is safe.

To implement this warning on types with `Opaque` without triggering the warning on other normal first-order types, `define-predicate` passes an `exact?` argument as true when generating the contract. If `type->static-contract` finds an `Opaque` type when `exact?` is true, it triggers the warning.

# Drawbacks and Alternatives
[drawbacks]: #drawbacks

The main reason *not* to do this is backwards compatibility for programs that use the negative proposition in the predicates for opaque types. Programs like this:
```racket
(require/typed m [#:opaque Foo foo?])
(: f (-> (U Foo String) String))
(define (f x)
  (cond [(foo? x) ""]
        [else     x]))
```
which typechecked before, will no longer typecheck since in the else case `x` still has type `(U Foo String)` rather than being refined to `String`.

There are alternative ways to close the unsoundness shown in Motivation and https://github.com/racket/typed-racket/issues/457. The unsoundness is caused by two things together: (1) exact decisions yes/no from predicates, and (2) mutation changing predicate answers. This RFC addresses it by changing (1), so an alternative is to leave (1) alone and fix (2) instead. This could be done by enforcing purity, although it would have to enforce purity in untyped code as well, which is impractical. An less-restrictive property to enforce is "no take-backs", which could be done with contract wrappers that memoize every input-output pair, raising an error if an output is different from the previous outputs for the same inputs.

# Prior art
[prior-art]: #prior-art

Prior versions of types based on predicates usually use the predicate as an exact decision yes/no, and rely on the assumption that the results of the predicate don't change. In pure languages such as Haskell, ACL2, or Coq, this is a valid assumption. Other languages such as Dafny constrain predicates to a subset of the language that restricts reading or writing state unless specifically annotated. This allows the compiler to use the predicate as an exact decision only for the region in which no state changes occur.

However, Typed Racket cannot enforce those restrictions on all opaque prediactes because they often come from untyped code which the typechecker has no control over.

# Unresolved questions
[unresolved]: #unresolved-questions

I have three unresolved questions:
  1. Design: is changing the existing `Opaque` and `#:opaque` for soundness preferable even if it's not strictly backwards compatible, or would it better to adding entirely new sound versions?
  2. Implementation: to distinguish types that provide exact decision yes/no predicates (`define-predicate` works with no warning) from types that provide only positive yes/unknown predicates (`define-positive-predicate` works, but `define-predicate` produces a warning), is passing a new `exact?` argument to contract-generation good, or is there a better way to implement it?
  3. Timing: after the warning is put in place on uses of `define-predicate` with `Opaque` types, how long should we wait until making it an error to fully close the unsoundness? 1 release cycle, 2 release cycles, or more?
