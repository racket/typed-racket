#lang scribble/manual

@begin[(require "../utils.rkt"
		scribble/core scribble/example
		(for-label (only-meta-in 0 typed/racket)))]

@(define the-eval (make-base-eval))
@(the-eval '(require typed/racket))

@title[#:tag "types"]{Types in Typed Racket}

Typed Racket provides a rich variety of types to describe data. This
section introduces them.

@section{Basic Types}

The most basic types in Typed Racket are those for primitive data,
such as @racket[True] and @racket[False] for booleans, @racket[String]
for strings, and @racket[Char] for characters.

@examples[#:label #f #:eval the-eval
'"hello, world"
#\f
#t
#f]

Each symbol is given a unique type containing only that symbol.  The
@racket[Symbol] type includes all symbols.

@examples[#:label #f #:eval the-eval
'foo
'bar]

Typed Racket also provides a rich hierarchy for describing particular
kinds of numbers.

@examples[#:label #f #:eval the-eval
0
-7
14
3.2
7+2.8i]

Finally, any value is itself a type:

@examples[#:label #f #:eval the-eval
(ann 23 23)]

@section{Function Types}

We have already seen some examples of function types.  Function types
are constructed using @racket[->], where the last type is the result
type and the others are the argument types.
Here are some example function types:

@racketblock[
(-> Number Number)
(-> String String Boolean)
(-> Char (Values String Natural))
]

The first type requires a @racket[Number] as input, and produces a
@racket[Number].  The second requires two arguments.  The third takes
one argument, and produces  @rtech{multiple values}, of types
@racket[String] and @racket[Natural].  Here are example functions for
each of these types.

@examples[#:label #f #:eval the-eval
(lambda ([x : Number]) x)
(lambda ([a : String] [b : String]) (equal? a b))
(lambda ([c : Char]) (values (string c) (char->integer c)))]


@section{Types for Functions with Optional or Keyword Arguments}

Racket functions often take optional or keyword arguments in addition
to standard mandatory arguments. Types for these functions can be written
concisely using the @racket[->*] type constructor. Here are some
examples:

@racketblock[
(->* () (Number) Number)
(->* (String String) Boolean)
(->* (#:x Number) (#:y Number) (values Number Number))
]

The first type describes a function that has no mandatory arguments,
one optional argument with type @racket[Number], and returns a
@racket[Number].

The second requires two mandatory arguments, no optional arguments,
and produces a @racket[Boolean]. This function type could have been
written using @racket[->] as @racket[(-> String String Boolean)].

The third requires a mandatory keyword argument with the keyword
@racket[#:x] and accepts an optional argument with keyword @racket[#:y].
The result is two values of type @racket[Number].

@;; FIXME: examples for these are not great right now because
@;; the lambda: does not allow optional arguments and lambda does
@;; not allow type annotations. This will be fixed in the future,
@;; so update the Guide then.

@section{Union Types}

Sometimes a value can be one of several types.  To specify this, we
can use a union type, written with the type constructor @racket[U].

@examples[#:label #f #:eval the-eval
(let ([a-number 37])
  (if (even? a-number)
      'yes
      'no))]

Any number of types can be combined together in a union, and nested
unions are flattened.

@racketblock[(U Number String Boolean Char)]

@section{Recursive Types}

@deftech{Recursive types} are types whose definitions refer to
themselves.  This allows a type to describe an infinite family
of data.  For example, this is the type of binary trees of numbers.

@margin-note[]{
Recursive types can also be created anonymously without the use of
@racket[define-type] using the @racket[Rec] type constructor.}

@racketblock[
(define-type BinaryTree (U Number (Pair BinaryTree BinaryTree)))]

Types can also be @emph{mutually recursive}. For example, the above
type defintion could also be written like this.

@racketblock[
(define-type BinaryTree (U BinaryTreeLeaf BinaryTreeNode))
(define-type BinaryTreeLeaf Number)
(define-type BinaryTreeNode (Pair BinaryTree BinaryTree))]

Of course, all recursive types must pass the contractivity check. In other
words, types which directly refer to themselves are not permitted. They must be
used as arguments to productive type constructors, such as @racket[Listof] and
@racket[Pairof]. For example, of the following definitions, only the last is
legal.

@examples[#:label #f #:eval the-eval
(eval:error (define-type BinaryTree BinaryTree))
(eval:error (define-type BinaryTree (U Number BinaryTree)))
(define-type BinaryTree (U Number (Listof BinaryTree)))]

@section{Structure Types}

Using @racket[struct] introduces new types, distinct from any
previous type.

@racketblock[(struct point ([x : Real] [y : Real]))]

Instances of this structure, such as @racket[(point 7 12)], have type @racket[point].

If a struct supertype is provided, then the newly defined type
is a @tech{subtype} of the parent.

@section{Types for Structure Type Properties}

To annotate a new structure type property created by
@racket[make-struct-type-property], it must be defined via
@racket[define-values] at the top level or module level:

@examples[#:eval the-eval #:label #f
    (: prop:foo (Struct-Property (-> Self Number)))
    (: foo-pred (-> Any Boolean : (Has-Struct-Property prop:foo)))
    (: foo-accessor (-> (Has-Struct-Property prop:foo)
                        (Some (X) (-> X Number) : #:+ X)))
    (define-values (prop:foo foo-pred foo-accessor)
                   (make-struct-type-property 'foo))]

@racket[Struct-Property] creates a type for a structure type property
descriptor and its argument is the expected type for property values. In
particular, when a structure type property expects a function to be applied
with the receiver, a structure instance the property value is extracted from,
@racket[Self] is used to denotes the receiver type. For a value in supplied in
a @racket[struct] definition for such a property, we use the structure type for
a by-position parameter for @racket[Self]:

@examples[#:eval the-eval #:label #f
  (eval:no-prompt (struct apple ([a : Number])
                     #:property prop:foo
                     (lambda ([me : apple]) : Number
                         (apple-a me))))
]

A property @seclink["propositions-and-predicates"]{predicate} tells the
arguments variable is a @racket[Has-Struct-Property] if the predicate check
succeeds. @racket[Has-Struct-Property] describes a
@seclink["Subtyping"]{subtyping} relation between structure types and
properties attached to them. In the example above, @racket[apple] is a subtype
of @racket[(Has-Struct-Property prop:foo)]


For a property accessor procedure, the argument must have a
@racket[Has-Struct-Property] type. If a property expects a value to be a
function called with the receiver, i.e. @racket[Self] appears in the type of
the corresponding property descriptor, an @tech[#:doc '(lib
"typed-racket/scribblings/ts-reference.scrbl") #:key "Some"]{existential type
result} is required. Its quantifier needs to correspond to @racket[Self] and
also appear in the @racket[proposition]. Such a return type ensures that the
extracted function cannot be called with another instance of the structure type
or substructure types other than the receiver:

@examples[#:eval the-eval #:label #f
  (let ([a1 : apple (apple 42)])
    ((foo-accessor a1) a1))

  (eval:error
    (let ([a1 : apple (apple 42)])
      ((foo-accessor a1) (apple 10))))
]

Otherwise, the return type should be the same as the type argument to
@racket[Struct-Property] for the descriptor.


@section{Subtyping}

In Typed Racket, all types are placed in a hierarchy, based on what
values are included in the type.  When an element of a larger type is
expected, an element of a smaller type may be provided.  The smaller
type is called a @deftech{subtype} of the larger type.  The larger
type is called a @deftech{supertype}. For example,
@racket[Integer] is a subtype of @racket[Real], since every integer is
a real number.  Therefore, the following code is acceptable to the
type checker:

@racketblock[
(: f (-> Real Real))
(define (f x) (* x 0.75))

(: x Integer)
(define x -125)

(f x)
]

All types are subtypes of the @racket[Any] type.

The elements of a union type are individually subtypes of the whole
union, so @racket[String] is a subtype of @racket[(U String Number)].
One function type is a subtype of another if they have the same number
of arguments, the subtype's arguments are more permissive (is a supertype), and the
subtype's result type is less permissive (is a subtype).
For example, @racket[(-> Any String)] is a subtype of @racket[(-> Number
(U String #f))].

@;@section{Occurrence Typing}

@section{Polymorphism}

Typed Racket offers abstraction over types as well as values. This allows
the definition of functions that use @deftech{parametric polymorphism}.

@subsection{Type Constructors}

Types for built-in collections are created by
@tr-reference-seclink["built-in-type-constructors"]{built-in type constructors}.
Users can also define their own type constructors through @racket[define-type].

Note that types and type constructors are different. If a type constructor is
used in a position where a type, the type checker will report a type error:
@examples[#:eval the-eval #:label #f
  (eval:error (ann 10 (Listof Listof)))
]

Conversely, types cannot be used as type constructors:
@examples[#:eval the-eval #:label #f
  (eval:error (ann 10 (Number Number)))
]

@subsection{Polymorphic Data Structures}

Virtually every Racket program uses lists and other collections.  Fortunately, Typed
Racket can handle these as well.  A simple list processing program can be
written like this:

@racketmod[
typed/racket
(: sum-list (-> (Listof Number) Number))
(define (sum-list l)
  (cond [(null? l) 0]
        [else (+ (car l) (sum-list (cdr l)))]))
]

This looks similar to our earlier programs --- except for the type
of @racket[l], which looks like a function application.  In fact, it's
a use of the @italic{type constructor} @racket[Listof], which takes
another type as its input, here @racket[Number].  We can use
@racket[Listof] to construct the type of any kind of list we might
want.

We can define our own type constructors as well.  For example, here is
an analog of the @tt{Maybe} type constructor from Haskell:

@racketmod[
typed/racket
(struct Nothing ())
(struct (A) Just ([v : A]))

(define-type (Maybe A) (U Nothing (Just A)))

(: find (-> Number (Listof Number) (Maybe Number)))
(define (find v l)
  (cond [(null? l) (Nothing)]
        [(= v (car l)) (Just v)]
        [else (find v (cdr l))]))
]

The first @racket[struct] defines @racket[Nothing] to be
a structure with no contents.

The second definition

@racketblock[
(struct (A) Just ([v : A]))
]

creates a type constructor, @racket[Just], and defines a namesake structure with
one element, whose type is that of the type argument to @racket[Just].  Here the
type parameters (only one, @racket[A], in this case) are written before the type
name, and can be referred to in the types of the fields.

The type definiton
@racketblock[
  (define-type (Maybe A) (U Nothing (Just A)))
]
creates a type constructor --- @racket[Maybe] is a potential
container for whatever type is supplied.

The @racket[find] function takes a number @racket[v] and list, and
produces @racket[(Just v)] when the number is found in the list,
and @racket[(Nothing)] otherwise.  Therefore, it produces a
@racket[(Maybe Number)], just as the annotation specified.

@subsection{Polymorphic Functions}

Sometimes functions over polymorphic data structures only concern
themselves with the form of the structure.  For example, one might
write a function that takes the length of a list of numbers:

@racketmod[
typed/racket
(: list-number-length (-> (Listof Number) Integer))
(define (list-number-length l)
  (if (null? l)
      0
      (add1 (list-number-length (cdr l)))))]

and also a function that takes the length of a list of strings:

@racketmod[
typed/racket
(: list-string-length (-> (Listof String) Integer))
(define (list-string-length l)
  (if (null? l)
      0
      (add1 (list-string-length (cdr l)))))]

Notice that both of these functions have almost exactly the same
definition; the only difference is the name of the function.  This
is because neither function uses the type of the elements in the
definition.

We can abstract over the type of the element as follows:

@racketmod[
typed/racket
(: list-length (All (A) (-> (Listof A) Integer)))
(define (list-length l)
  (if (null? l)
      0
      (add1 (list-length (cdr l)))))]

The new type constructor @racket[All] takes a list of type
variables and a body type.  The type variables are allowed to
appear free in the body of the @racket[All] form.

@subsection{Lexically Scoped Type Variables}

When the @racket[:] type annotation form includes type variables
for @tech{parametric polymorphism},
the type variables are @emph{lexically scoped}.
In other words, the type variables are bound in the body of the
definition that you annotate.

For example, the following definition of @racket[_my-id] uses
the type variable @racket[_a] to annotate the argument
@racket[_x]:

@racketblock[
(: my-id (All (a) (-> a a)))
(define my-id (lambda ([x : a]) x))
]

Lexical scope also implies that type variables can be shadowed,
such as in the following example:

@racketblock[
(: my-id (All (a) (-> a a)))
(define my-id
  (lambda ([x : a])
    (: helper (All (a) (-> a a)))
    (define helper
      (lambda ([y : a]) y))
    (helper x)))
]

The reference to @racket[_a] inside the inner @racket[lambda]
refers to the type variable in @racket[_helper]'s annotation.
That @racket[_a] is @emph{not} the same as the @racket[_a]
in the annotation of the outer @racket[lambda] expression.


@(close-eval the-eval)

@include-section["varargs.scrbl"]

@;@section{Refinement Types}
