- Feature Name: Typing Generic Intefaces
- Start Date: 2/23/2021
- RFC PR:
- Feature Commit(s):

# Summary

This RFC aims to provide almost complete support for typechecking generic interfaces.

# Motivation
The Typed Racket type checker currently doesn't support generic interfaces,
which has been one of the most wanted features.


# Guide-level explanation
There are two parts of the proposal: First, we address typing declarations of
generic interfaces. Then, we show how the type checker checks structs' method
definitions.

To annotate a generic interface, we introduce a typed counterpart for
`define/generics`.

```racket
(define-generics showable
  (: gen-show (showable . -> . String) )
  (gen-show showable)
  (: gen-show2 (showable showable . -> . String) )
  (gen-show2 some-b showable)
  #:defined-predicate tpred ;; (: tpred (-> showable (U 'gen-show 'gen-show2) * Boolean)). Note: No need to generate contracts
  #:defaults
  ([string? (define (gen-show s) ;;
              (format "default string : ~a" s))
            (define (gen-show2 _ s)
              (format "default string 2: ~a" s))]
   [symbol? (define (gen-show s)
              (format "default symbol : ~a" s))
            (define (gen-show2 _ s)
              (format "default symbol 2: ~a" s))])
  #:fast-defaults
  ([string? (define (gen-show s)
              (format "fast default string : ~a" s))
            (define (gen-show2 _ s)
              (format "fast default string 2: ~a" s))]
   [number? (define (gen-show n)
              (format "fast default number : ~a" n))
            (define (gen-show2 _ n)
              (format "fast default number 2: ~a" n))]))

```
Apart from what Racket's `define-generics` offers, the typed
`define-generics` will do the following:
- introduce a generic interface type `showable`.
- require users to annotate the generic functions. Note that the `formals` seems
redundant, but they are necessary. This is because Racket needs a single
required by-position argument to dispatch methods, and the macro would not be
able to derive that argument for `formals` only from types. For example, we
cannot tell which argument would act as the specializer based on the type of
`gen-show2`.
- generate the type for a `defined-predicate-id`. In this case, the
  defined-predicate-id `tpred` should have type `(-> showable (U 'gen-show
  'gen-show2) * Boolean)`
- typecheck implemented method in `defaults` and `fast-defaults`. In each of
  their entries, the type of an implemented method should preserve the subtyping
  property for functions. Specifically, the specializer argument should be of a
  super type of what the preceding type predicate is for.
- typecheck methods in the `fallbacks` section. Note that since Racket does not
  support generic inheritance in any kind or shape, the specializer argument's
  type can only the generic interface type.
- produce a typed immutable hash table that is assigned to `defined-table-id`
  when it is specified. The table's keys have a union type, `(U
  method-id-as-symbol ...)`, and values are simply booleans.
- #:derive-property, TODO

For method implementation in a struct's definition, the typechecking process is
also straightforward

```
(struct
  point (x)
  #:methods gen:showable
  [(define/generic super-show gen-show)
   (define (gen-show me)
     (format "x is ~a" (super-show (point-x me))))
   (define (gen-show2 _ me)
     (format "x is ~a" (super-show (point-x me))))]
  )
```

First, the typechecker ensures every `method-id` in a `#:methods` specification
is well scoped.  Then it checks if the specializer argument's and return type
are covariant and if the rest are contraviant.  `define/generic` makes the local
id `super-show` have the same type of `gen-show`, namely `(-> showable String)`.

Though Racket doesn't support subclass or inheritance between generic
interfaces, we can still express constraints using types in `define/generics`.

```
(define-generics eq-able
  (: gen-== (eq-able eq-able . -> . Boolean) )
  (gen-== eq-able e)
  (: gen-/= (eq-able eq-able . -> . Boolean) )
  (gen-/= eq-able e)
  #:fallbacks [(define/generic super-== gen-==)
               (define/generic super-/= gen-/=)
               (define (gen-== eq-able o)
                 (not (super-/= eq-able o)))
               (define (gen-/= eq-able o)
                 (not (super-== eq-able o)))])

(define-generics ord-able
  (: gen-< (Inter eq-able ord-able) (Inter eq-able ord-able) . -> . Boolean)
  (gen-< ord-able o)
  (: gen-<= (Inter eq-able ord-able) (Inter eq-able ord-able) . -> . Boolean)
  (gen-<= ord-able o)
  (: gen-> (Inter eq-able ord-able) (Inter eq-able ord-able) . -> . Boolean)
  (gen-> ord-able o)
  (: gen->= (Inter eq-able ord-able) (Inter eq-able ord-able) . -> . Boolean)
  (gen->= ord-able o)
  #:fallbacks [(define/generic super-<= gen-<=)
               ;; (: gen-< (-> (Inter orderable eq-able) (Inter orderable eq-able) Boolean))
               (define (gen-< orderable o)
                 (and (super-<= orderable o)
                 (gen-/= orderable o)))])
```

Here we want to implement the idea that an `ord-able` structure is also
`eq-able`.  Those types above enforce a stronger restraint than the untyped
order-able. Because in untyped code, a struct that implements `ord-able` is not
obligated to implement `eq-able`. Consider the following code:

```
(struct dummy ([v : Any])
  #:methods gen:ord-able
  [(define (gen-< [me : Dummy] o)
     (and #t
          (gen-/= me o)))])

(gen-< (dummy 10) (dummy "10"))
```

If we turn the code above into untyped code and then run it, a run-time type
error will raise during the invocation of `(gen-< (dummy 10) (dummy "10"))`,
because an `Dummy` instance breaks the contract of `gen-/=`.

However, the typechecker simply rejects the code. Since `Dummy` does not
implement `gen:eq-able`, it is not of a subtype of `(Intersection eq-able ord-able)`

# Reference-level explanation
Add a new prim for `define-generics` that supports the features mentioned in the
Guide-level explanation.

Modify the `-struct` prim to support `#:methods`. Add code to check method
implementation in pass 1 and pass 2 accordingly

# Drawbacks and Alternatives
[drawbacks]: #drawbacks

## Drawbacks
Adds complexity to the type checker, but it seems that the type system will be untouched.

# Prior art
[prior-art]: #prior-art

As of Racket 8.0, generic interaces are not supported in Typed Racket.

# Unresolved questions
[unresolved]: #unresolved-questions
