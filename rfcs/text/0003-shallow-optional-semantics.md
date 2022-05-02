- Feature Name: shallow-optional-semantics
- Start Date: 2020-07-21
- RFC PR: (leave this empty)
- Feature Commit(s): (leave this empty)

# Summary

This RFC adds two new languages to the Typed Racket (TR) world: Shallow Typed
Racket and Optional Typed Racket. By contrast to normal Typed Racket --- which
we'll call Deep Typed Racket --- the new languages have "weaker" types. They
do less work at run-time to protect typed code from untyped Racket code.

All three languages (Shallow, Optional, and Deep) use the same syntax for types
and the same type checker. The only difference is what types mean (and cost) at
run-time.

For example, let's begin with the static type `(Listof String)` and ask what
untyped values are allowed to have this type in the different languages. In
other words, the question is what values of `str*` can satisfy the run-time
requirements of the declaration `(: str* (Listof String))`:

1. In Deep TR (`#lang typed/racket` or `#lang typed/racket/deep`), the value
   `str*` must be a list of strings.

2. In Shallow TR (`#lang typed/racket/shallow`), the value `str*` must be a
   list. There are no constraints on the elements of the list; they don't need
   to be strings.
     That being said, if typed code reads the first element of the list and
   expects a `String`, then Shallow TR will make sure that one element is a
   string.

3. In Optional TR (`#lang typed/racket/optional`), `str*` can be any value.


In general, Deep types give strong guarantees. Shallow types give weaker
"surface-level" guarantees. Optional types give no guarantees.

Shallow and Optional types are useful, though, because they are weaker.
They add a lower run-time cost for interactions with untyped Racket code
(zero cost for Optional!) and have simpler behavior (examples in the rest
of this document illustrate).


## Document Outline

In the rest of this RFC, the focus is on Shallow types and how we plan to
implement them using the _Transient_ semantics. The idea with Transient is
to rewrite all typed code with simple assertions about the shape of values.

By contrast to Transient, normal TR gets Deep types through the Guarded
(aka Natural) semantics. The idea with Guarded is to keep a strict boundary
between typed and untyped code with flat contracts, chaperones, and
impersonators.

The Optional idea is simple: use the TR type checker at compile-time and normal
`#lang racket` behavior at run-time. It is simlar to the no-check language,
but actually runs the type checker.


# Motivation

Deep types are expensive and difficult to enforce. Shallow types via the
Transient semantics are often cheaper and always easier to check. Changing a
program to use Shallow types may give two immediate benefits:

  S1. A program should run faster if it heavily mixes typed and untyped code.

  S2. Some type-correct programs that cannot run in Deep can run in Shallow.

Deep types still have benefits over Shallow types:

  D1. When a typed/untyped interaction goes wrong, Deep types blame a
      precise boundary between a typed module and an untyped one.
      Shallow types cannot be so precise.

  D2. Fully-typed (and mostly-typed) Deep programs often run faster than
      untyped, but Shallow programs always run slower with more types.

Points S1 and S2 motivate Shallow. Points D1 and D2 motivate a mix of Deep
and Shallow. The goal is to offer both as `#lang`s that can interact.


### (S1) Shallow types can be faster

These comments are about Shallow types as implemented by Transient.

#### Example 1 : read-only data

When an untyped list flows into Deep code, every element in the list gets a
runtime check.  When the same list flows to Shallow code, there is a simple
`list?` check only; other checks come as needed. So, simple list functions
should run much faster.

  ```
  (: get-first (-> (Listof String) String))
  (define (get-first str*)
    (car str*))
  ```

If `get-first` lives in a `#lang typed/racket/deep` module, every call walks
the whole list. But if `get-first` is in a `#lang typed/racket/shallow` module,
only 1 element gets checked because only 1 element gets accessed.


### Example 2 : mutable data

When an untyped vector flows into Deep code, it gets wrapped in a chaperone
to make sure that its future behavior matches the type. When a vector flows
into Shallow code, there is one `vector?` check. Shallow does not add a
wrapper and element checks happen only when needed.

Example:

  ```
  (: vector-id (-> (Vectorof Real) (Vectorof Real)))
  (define (vector-id v)
    v)
  ```

With Deep types, the incoming vector gets wrapped in a chaperone. After this
vector flows out, all future uses of it need to go through the chaperone, so
those future uses suffer indirection and checking costs.

With Shallow types, this code has one `vector?` check. The vector `v` does
not get a wrapper.


### Example 3 : functions

Deep types wrap and chaperone functions just like mutable data. Shallow types
spot-check the result of function calls, but do not create wrappers.

This example makes a typed wrapper around the `racket/base` map
function:

  ```
  (: rmap (-> (-> Real Boolean) (Listof Real) (Listof Boolean)))
  (define (rmap f r*)
    (map f r*))
  ```

With Deep types, the function `f` gets a wrapper. When `map` uses this function,
the wrapper checks that `f` gets a `Real` and returns a `Boolean` for every
element in the list. Deep also pre-emptively checks the list `r*` before
shipping it to `map`.

With Shallow types, there are two checks: does `f` look good? and does `r*` look
good? These checks are basically `procedure?` and `list?`. That's all.


### (S2) Shallow types can allow new interactions

Deep types need to use wrappers to check/protect mutable values. Every kind of
mutable value in Racket needs a custom kind of wrapper. But some wrappers do
not exist yet, and so TR conservatively rejects some programs.

For example, mpairs are mutable and do not have a wrapper. Thus, the following
"good" program gives a runtime error with Deep types:

  ```
  #lang racket

  (module t typed/racket
    (: add-mpair (-> (MPairof Real Real) Real))
    (define (add-mpair mp)
      (+ (mcar mp) (mcdr mp)))
    (provide add-mpair))

  (require 't)

  (add-mpair (mcons 2 4))
  ;; Type Checker: could not convert type to a contract;
  ;; contract generation not supported for this type
  ```

Shallow types (via Transient) can run the program. For Shallow type safety, the
typed function checks `mpair?` of its input and `real?` after the getter
functions.

Syntax objects also lack wrappers. (Wrappers are needed for Deep types because
a syntax object may contain a mutable value.) Implementing these wrappers would
require changes to basic parts of Racket including the macro expander.


### (D1) Deep types give better errors

Thanks to the heavy checking and careful use of wrappers, Deep types give a
strong guarantee: every communication between typed and untyped code will
either match the types or stop the program. In other words, there is no way
that untyped code can sneak a bad value into typed code.

Because of this Deep-types guarantee (called "complete monitoring" in the
papers), TR can blame one _correct_ source-code boundary when a check fails.
The boundary is to blame in the sense that something is wrong on at least
one of its sides: either the untyped code produced a bad value, or typed code
has a bad expectation.

Shallow types cannot always blame a single boundary because they rarely check
a boundary completely. Let's go back to lists; here is a typed function that
looks at part of a list and passes it on:

  ```
  (: spot-check (-> (Listof Symbol) (Listof Symbol)))
  (define (spot-check sym*)
    (cons (cadr sym*) (cons (car sym*) (cddr sym*))))
  ```

With Shallow types, there is no guarantee that the result is a list of symbols.
Suppose that we start with a list `'(A B "C")` and it goes through this
function and later crosses several typed/untyped boundaries and finally
some typed code realizes the 3rd element is a string. Unless we keep a record
of every place the list has been, there is no way to point back to the first
`spot-check` call. There is also no reason the first boundary should always
be blamed.

Shallow-type blame gets even worse when types make claims about untyped
libraries. There might be no blame at all. In the example below, the types in
the middle incorrectly say that the library on top sends numbers to its
callback. The client on the bottom assumes the types are right. If the
types are Shallow, they do not protect the client from unexpected input:

  ```
  #lang racket

  (module library racket
    (define (sender f)
      (f "hello"))
    (provide sender))

  (module types typed/racket/shallow
    (require/typed/provide (submod ".." library)
      (sender (-> (-> Real Real) Real))))

  (module client racket
    (require (submod ".." types))
    (sender (lambda (n) (add1 n))))

  (require 'client)
  ;; Shallow => add1 contract violation
  ```

In Shallow TR, a type mismatch leads to a simple assertion failure. Shallow TR
does not try to trace the error back to a boundary (or set of boundaries).

There is a method for improving blame in Transient that Shallow TR could use,
but we have found it is too slow. More information can be found here:

  <http://cs.brown.edu/people/bgreenma/publications/publications.html#gldf-pj-2022>


### (D2) Adding types affects Deep and Shallow differently

Deep types check the boundaries between typed and untyped code. If there are
no boundaries, there are no checks.

Shallow types via Transient pre-emptively protect typed code. Every expression
in typed code may get compiled to have a check. It all depends on whether the
code can receive untyped input.

Early experience with Deep and Shallow TR suggests a few general claims
about how adding types changes performance:

- Deep types can add a huge slowdown when typed and untyped code share
  mutable data.

- Deep types also enable optimizations. If there are no crippling typed/untyped
  boundaries in a program, then a typed version is often faster than untyped.

- Shallow types add a low, steady overhead as more code gets typed.

- Shallow types enable some optimizations, but these rarely outweigh the cost
  of the checks.

Chapter 6 of Greenman's dissertation has more information:

  <http://cs.brown.edu/people/bgreenma/publications/publications.html#g-dissertation-2020>


# Guide-level explanation

See `Summary` section.


# Reference-level explanation

## 8 Typed Racket Syntax With Shallow Types

(The reference will use parts of the motivation above, a short description
 like the one for optional typing below, and maybe some tips about when
 to use Shallow types)


## 9 Typed Racket Syntax With Optional Types

The `typed/racket/optional` and `typed/racket/base/optional` languages provide
the same bindings as `typed/racket` and `typed/racket/base` and use the same
type-checking, but have the same run-time behavior as `racket` and
`racket/base`. Typed Racket types are normally true claims about the ways
that a program can behave, but in these optional languages the types say
nothing about behavior.

Optional typing is useful if you want the type-checker to spot-check your
program and are willing to deal with arbitrary Racket behaviors. For example,
an optionally-typed function with type `(-> Integer Integer)` could receive
a string as input and return a symbol.


# Drawbacks and Alternatives
[drawbacks]: #drawbacks

Drawback: more languages = more choices = more ways to get confused.
And people may be surprised that typed-to-typed communication is still
 expensive (because Deep needs to protect itself against Shallow types).

One alternative is to keep trying to make TR faster.

Another is to implement Shallow types using wrappers instead of the Transient
strategy. With wrappers, interaction with TR should be faster. But doing may
require new contracts (or chaperones), and will prevent some typed/untyped
mixes that Transient allows (unless we can make new chaperones to allow them).


# Prior art
[prior-art]: #prior-art

Shallow types are in Reticulated Python and Grace.

Reticulated is the original home of the Transient semantics. Michael Vitousek
invented Transient; his dissertation talks about experiences with it
(especially in Chapter 4).

Grace has shallow checks that were inspired by Transient.


Resources for transient semantics (Shallow):
- <https://scholarworks.iu.edu/dspace/handle/2022/23172>
- <https://2019.ecoop.org/details/ecoop-2019-papers/15/Transient-Typechecks-are-Almost-Free>

Resources that compare Deep and Shallow types:
- <http://prl.ccs.neu.edu/blog/2018/12/11/the-behavior-of-gradual-types-a-user-study/>
- <http://prl.ccs.neu.edu/blog/2019/10/31/complete-monitors-for-gradual-types/>

Resources that talk about many ways of mixing typed and untyped code:
- <http://prl.ccs.neu.edu/blog/2018/10/06/a-spectrum-of-type-soundness-and-performance/>
- <http://ccs.neu.edu/home/types/publications/publications.html#gfd-oopsla-2019>


# Unresolved questions
[unresolved]: #unresolved-questions

PR #948 has a list of lower-level todo items.


> - What parts of the design do you expect to resolve through the RFC process
>   before this gets merged?

Documentation. What should it include, and how should it be organized to
introduce new TR languages.


> - What parts of the design do you expect to resolve through the implementation
>   of this feature before stabilization?

1. [RESOLVED] How to insert checks everywhere. Today, RackUnit `test-case`s and
   `with-handlers` blocks are unsafe because they're marked as "ignored" by TR.
   There may be others. (2020-07-26)

2. What's the ideal check for every type?
   Should `(Listof T)` use `list?` or `(or/c null? pair?)`?
   Should `(-> Void Void)` use `procedure?` or
    `(and/c procedure? (procedure-arity-includes/c 1))`?
   So far I've gone for "complete" checks to help the programmer and TR
    optimizer, but if the cost is too high let's go simpler.

3. [RESOLVED] How to minimize the cost of each check, probably by avoiding
   `racket/contract` combinators.


> - What related issues do you consider out of scope for this RFC that could be
>   addressed in the future independently of the solution that comes out of this
>   RFC?

Below are 4 issues related to the transient semantics.


- How to add blame? For now, an error gives a failed check (value, type,
  srcloc) and a stack trace. It may be helpful to give some blame info.
  Then again, programmers can always switch to Deep TR for precise blame.

- How to avoid redundant checks (via static/dynamic analysis)? For example, a
  function that takes the `car` of a pair twice currently pays 2 checks.

  ```
    (define (f (xy : (Pairor Real Real))) : Real
      (+ (car xy) (car xy))

      #;(+ (check real? (car xy)) (check real? (car xy))))
  ```

