#lang scribble/manual

@begin[(require "../utils.rkt" scribble/example racket/sandbox racket/list)
       (require (only-in scribble/eval interaction-eval interaction/no-prompt))
       (require (for-label typed/untyped-utils (only-meta-in 0 [except-in typed/racket for])))]

@(define-syntax-rule (module-example #:eval ev #:label lbl lang-datum mod-code ... ex-code)
   (list
     (if lbl (list lbl) '())
     (racketmod lang-datum mod-code ...)
     (interaction-eval #:eval ev mod-code ...)
     (interaction/no-prompt #:eval ev ex-code)))

@(define deep-eval (make-base-eval #:lang 'typed/racket))
@(define shallow-eval (make-base-eval #:lang 'typed/racket/shallow))
@(define optional-eval (make-base-eval #:lang 'typed/racket/optional))

@title[#:tag "behavior-of-types"]{Deep, Shallow, and Optional Semantics}

@(define-syntax-rule @defmodulelang/hidden[name]
   (list
      (section #:style '(hidden toc-hidden unnumbered)
               (symbol->string 'name))
      @defmodulelang[name]))

@defmodulelang/hidden[typed/racket/deep]
@defmodulelang/hidden[typed/racket/base/deep]
@defmodulelang/hidden[typed/racket/shallow]
@defmodulelang/hidden[typed/racket/base/shallow]
@defmodulelang/hidden[typed/racket/optional]
@defmodulelang/hidden[typed/racket/base/optional]

@margin-note{See also: @secref["typed-untyped-interaction" #:doc '(lib
"typed-racket/scribblings/ts-guide.scrbl")] in the Typed Racket Guide.}
Typed Racket allows the combination of both typed and untyped code in
a single program.
Untyped code can freely import typed identifiers.
Typed code can import untyped identifiers by giving them types (via
@racket[require/typed]).

Allowing typed/untyped combinations raises questions about @emph{whether} and @emph{how} types should
constrain the behavior of untyped code. On one hand, strong type constraints are useful
because they can detect when a typed-untyped interaction goes wrong. On the other hand,
constraints must be enforced with run-time checks, which affect run-time
performance. Stronger constraints generally impose a higher performance cost.

By default, Typed Racket provides @emph{Deep} types that strictly constrain the
behavior of untyped code. But because these constraints can be expensive,
Typed Racket offers two alternatives: @emph{Shallow} and
@emph{Optional} types.
All three use the same static types and static checks, but
they progressively weaken the run-time behavior of types.

@(define (langs-from . lang-name*)
   (add-between
     lang-name*
     (list ", ")
     #:splice? #true
     #:before-first (list "Available in: ")
     #:before-last (list ", and ")
     #:after-last (list ".")))

@itemlist[
@item{
  @emph{Deep} types enforce strong, compositional guarantees.
  If a value is annotated with a Deep type, then all of its interactions with
  other code must match the type.
  For example, a value with the type @racket[(Listof String)] must be a list
  that contains only strings; otherwise, Typed Racket raises an error.

  @langs-from[@racketmodname[typed/racket] @racketmodname[typed/racket/base]
              @racketmodname[typed/racket/deep] @racketmodname[typed/racket/base/deep]]
}
@item{
  @emph{Shallow} types enforce the outer shape of values.
  For example, the Shallow type @racket[(Listof String)] checks only for lists --- it does not check
  whether the list elements are strings.
  This enforcement may seem weak at first glance, but Shallow types can work
  together to provide a decent safety net.
  If Shallow-typed code gets an element from a list and expects a
  @racket[String], then another check will make sure the element is really a string.

  @langs-from[@racketmodname[typed/racket/shallow] @racketmodname[typed/racket/base/shallow]]
}
@item{
  @emph{Optional} types enforce nothing and add zero run-time cost.
  These types are useful for finding bugs in typed code at compile-time, but they cannot
  detect interaction errors at run-time.

  @langs-from[@racketmodname[typed/racket/optional] @racketmodname[typed/racket/base/optional]]
}
]

@section{Example Interactions}

The examples below show how Deep, Shallow, and Optional change the run-time behavior (or, the semantics) of types.

@subsection{Checking Immutable Data: Importing a List}

When typed code imports an untyped list:
@itemlist[
  @item{Deep types check each element of the list at the boundary to untyped code;}
  @item{Shallow types check for a list, and check elements when they are accessed; and}
  @item{Optional types check nothing.}
]

The following examples import the function @racket[string->list], which returns a list of characters,
and use an incorrect type that expects a list of strings.
Both Deep and Shallow types catch the error at some point.
Optional types do not catch the error.

Deep types prevent a list of characters from entering typed code with the type @racket[(Listof String)]:

@module-example[#:eval deep-eval #:label #f
  typed/racket (code:comment "or #lang typed/racket/deep")

  (require/typed racket/base
    [string->list (-> String (Listof String))])
  (string->list "racket")
]

Shallow types allow a list of characters to have the type @racket[(Listof String)], but detect an
error if typed code reads an element from the list:

@module-example[#:eval shallow-eval #:label #f
  typed/racket/shallow

  (require/typed racket/base
    [string->list (-> String (Listof String))])

  (define lst (string->list "racket"))
  (first lst)
]

Optional types do not detect any error in this example:

@module-example[#:eval optional-eval #:label #f
  typed/racket/optional

  (require/typed racket/base
    [string->list (-> String (Listof String))])

  (define lst (string->list "racket"))
  (first lst)
]


@subsection{Checking Mutable Data: Importing a Vector}

When typed code imports an untyped vector:
@itemlist[
  @item{Deep types wrap the vector in a contract that checks future reads and writes;}
  @item{Shallow types check for a vector at the boundary, and check elements on demand (same as for lists); and}
  @item{Optional types check nothing.}
]

The following example imports @racket[make-vector] with an incorrect type that expects
a vector of strings as its output.
When @racket[make-vector] returns a vector of numbers instead, both Deep and Shallow
types catch the error when reading from the vector.
Optional types do not catch the error.

@module-example[#:eval deep-eval #:label "Deep catches a bad vector element:"
  typed/racket (code:comment "or #lang typed/racket/deep")

  (require/typed racket/base
    [make-vector (-> Integer (Vectorof String))])

  (define vec (make-vector 10))
  (vector-ref vec 0)
]

@module-example[#:eval shallow-eval #:label "Shallow catches a bad vector element:"
  typed/racket/shallow

  (require/typed racket/base
    [make-vector (-> Integer (Vectorof String))])

  (define vec (make-vector 10))
  (vector-ref vec 0)
]

@module-example[#:eval optional-eval #:label "Optional does not catch a bad element:"
  typed/racket/optional

  (require/typed racket/base
    [make-vector (-> Integer (Vectorof String))])

  (define vec (make-vector 10))
  (vector-ref vec 0)
]


@subsection{Checking Functions that Cross Multiple Boundaries}

Deep types can detect some errors that Shallow types miss,
especially when a program contains several modules.
This is because every module in a program can trust that every Deep type is a true claim,
but only the one module that defines a Shallow type can depend on the type.
In short, Deep types are @emph{permanent} whereas Shallow types are @emph{temporary}.

The following example uses three modules to create a situation where Deep types catch
an error that Shallow types miss.
First, the untyped module @racketmodname[racket/base] provides the standard @racket[string-length] function.
Second, a typed @racket[_interface] module imports @racket[string-length] with an incorrect type and reprovides with a new name: @racket[strlen].
Third, a typed client module imports @racket[strlen] with a correct type and calls it on a string.

Deep types raise an error when @racket[strlen] is called because of the incorrect type in the interface:

@module-example[#:eval deep-eval #:label #f
  typed/racket (code:comment "or #lang typed/racket/deep")

  (module interface typed/racket
    (require/typed racket/base
      [string-length (-> String Void)])
    (define strlen string-length)
    (provide strlen))

  (require/typed 'interface
    [strlen (-> String Natural)])
  (strlen "racket")
]

Shallow types do not raise an error because the interface type is not enforced for the outer client module:

@module-example[#:eval shallow-eval #:label #f
  typed/racket/shallow

  (module interface typed/racket/shallow
    (require/typed racket/base
      [string-length (-> String Void)])
    (define strlen string-length)
    (provide strlen))

  (require/typed 'interface
    [strlen (-> String Natural)])
  (strlen "racket")
]

Optional types do not raise an error either:

@module-example[#:eval optional-eval #:label #f
  typed/racket/optional

  (module interface typed/racket/optional
    (require/typed racket/base
      [string-length (-> String Void)])
    (define strlen string-length)
    (provide strlen))

  (require/typed 'interface
    [strlen (-> String Natural)])
  (strlen "racket")
]


@section{Forms that Depend on the Behavior of Types}

The following Typed Racket forms use types to create run-time checks.
Consequently, their behavior changes depending on whether types are Deep,
Shallow, or Optional.

Across these forms, the changes are roughly the same.
Deep types get enforced as (higher-order) contracts,
Shallow types get enforced as shape checks,
and Optional types get enforced with nothing.
The key point to understand is @emph{which} types get enforced at run-time.

@itemlist[
  @item{
    @racket[require/typed] imports bindings from another module and attaches
    types to the bindings. The attached types get enforced.
  }

  @item{
    @racket[cast] assigns a type to an expression. The assigned type gets enforced.
  }

  @item{
    @racket[with-type] creates a typed region in untyped code. Types at the boundary
    between this region and untyped code get enforced.
  }
]

The following forms modify the contracts that Deep Typed Racket generates.
Uses of these forms may need to change to accommodate Shallow and Optional
clients.

@itemlist[
  @item{
    @racket[require/untyped-contract] brings an identifier from Deep-typed code
    to untyped code using a subtype of its actual type.
    If the required identifier travels from untyped code to a Shallow or Optional
    client, this client must work with the subtype. A Deep client would be able
    to use the normal type.
  }
  @item{
    @racket[define-typed/untyped-identifier] accepts four identifiers to
    fine-tune its behavior for Deep, untyped, Shallow, and Optional clients.
  }
]


@subsection{Example: Casts in Deep, Shallow, and Optional}

To give one example of a form that depends on the behavior of types,
@racket[cast] checks full types in Deep mode, checks shapes in Shallow mode,
and checks nothing in Optional mode.

@examples[#:eval deep-eval #:label "Deep detects a bad cast:"
  (code:comment "#lang typed/racket")
  (code:comment "or #lang typed/racket/deep")
  (eval:error (cast (list 42) (Listof String)))
]

@examples[#:eval shallow-eval #:label "Shallow allows one bad cast but detects a shape-level one:"
  (code:comment "#lang typed/racket/shallow")
  (cast (list 42) (Listof String))
  (eval:error (cast (list 42) Number))
]

@examples[#:eval optional-eval #:label "Optional lets any cast succeed:"
  (code:comment "#lang typed/racket/optional")
  (cast (list 42) (Listof String))
  (cast (list 42) Number)
]


@section[#:tag "how-to-choose"]{How to Choose Between Deep, Shallow, and Optional}

Deep, Shallow, and Optional types have complementary strengths
and weaknesses.
Deep types give strong type guarantees and enable full type-directed optimizations,
but may pay a high cost at boundaries.
In particular, the costs for higher-order types are high.
Examples include @racket[HashTable], @racket[->*], and @racket[Object].
Shallow types give weak guarantees, but come at a lower cost.
The cost is constant-time for many types, including @racket[HashTable] and @racket[->*],
and linear-time for a few others such as @racket[U] and @racket[Object].
Optional types give no guarantees, but come at no cost.

Based on these tradeoffs, this section offers some advice about when to choose
one style over the others.


@subsection{When to Use Deep Types}

Deep types are best in the following situations:

@itemlist[
  @item{
    For large blocks of typed code, to take full advantage of type-directed
    optimizations within each block.
  }
  @item{
    For tightly-connected groups of typed modules, because Deep types pay no
    cost to interact with one another.
  }
  @item{
    For modules in which you want the types to be fully enforced,
    perhaps for predicting the behavior of typed-untyped interactions
    or for debugging.
  }
]


@subsection{When to Use Shallow Types}

Shallow types are best in the following situations:

@itemlist[
  @item{
    For typed code that frequently interacts with untyped code, especially
    when it sends large immutable values or higher-order values (vectors, functions, etc.)
    across boundaries.
  }

  @item{
    For large blocks of typed code that primarily uses basic values (numbers, strings, etc.)
    or monomorphic data structures.
    In such cases, Shallow types get the full benefit of type-directed
    optimizations and few run-time costs.
  }
  @item{
    For boundaries where Deep enforcement (via contracts) is too restrictive.
    For example, Deep code can never call a function that has the type
    @racket[Procedure], but Shallow can after a cast.
  }
  @item{
    For boundaries where Deep cannot convert the types to contracts,
    such as for a higher-order syntax object such as @racket[(Syntaxof (Boxof Real))].
  }
]


@subsection{When to Use Optional Types}

Optional types are best in the following situations:

@itemlist[
  @item{
    For typed-to-untyped migrations where performance needs
    to be predictable, because an Optionally-typed program behaves just like a
    Racket program that ignores all the types.
  }

  @item{
    For boundaries that neither Deep nor Shallow can express.
    For example, only Optional can use occurrence types at a boundary.
  }

  @item{
    For prototyping; that is, for testing whether an idea can type-check
    without testing whether it interacts well with untyped code.
  }
]


@subsection{General Tips}

@itemlist[
  @item{
    Deep, Shallow, and Optional use the same compile-time type checks, so
    switching a module from one style to another is usually a one-line change
    (to the @hash-lang[] line).
  }
  @item{
    When converting a Racket program to Typed Racket, try Deep types at first
    and change to Shallow if run-time performance becomes a bottleneck
    (or, if contract wrappers raise a correctness issue).
  }

]


@section{Related Gradual Typing Work}

Shallow Typed Racket implements the @emph{Transient} semantics for gradual
languages @cite["Programming-2022" "PLDI-2022"],
which was invented by Michael M. Vitousek @cite["RP:DLS-2014" "RP:POPL-2017" "RP:Vitousek-2019" "RP:DLS-2019"].
Transient protects typed code by rewriting it to defensively check the shape of values
whenever it calls a function, reads from a data structure, or otherwise receives input
that may have come from an untyped source. Because of the rewriting, Transient is able
to enforce type soundness without higher-order contracts.

Deep Typed Racket implements the standard semantics for gradual languages,
which is known variously as Guarded @cite{RP:POPL-2017},
Natural @cite{TOPLAS-2009}, and
Behavioral @cite{KafKa-2018}.
This @emph{Guarded} semantics eagerly checks untyped values when possible and otherwise
creates wrappers to defer checks.

Typed Racket uses the names ``Shallow'' and ``Deep'' rather than ``Transient''
and ``Guarded'' to emphasize the guarantees that such types provide instead
than the method used to implement these guarantees.
Shallow types provide a type soundness guarantee;
Deep types provide type soundness and complete monitoring @cite{OOPSLA-2019}.

Optional types are a widely-used approach to gradual typing,
despite their unsound support for typed-untyped interactions.
Optionally-typed languages include the following:
@hyperlink["https://www.typescriptlang.org"]{TypeScript},
@hyperlink["https://flow.org"]{Flow},
@hyperlink["http://mypy-lang.org"]{mypy},
and Typed Clojure @cite["ESOP-2016" "Bonnaire-Sergeant-2019"].

@close-eval[deep-eval]
@close-eval[shallow-eval]
@close-eval[optional-eval]

