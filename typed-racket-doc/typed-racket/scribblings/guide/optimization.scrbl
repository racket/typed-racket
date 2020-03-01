#lang scribble/manual

@begin[(require (for-label (only-meta-in 0 typed/racket))
		"../utils.rkt" (only-in "quick.scrbl" typed-mod))]

@title[#:tag "optimization"]{Optimization in Typed Racket}

@margin-note{For general information on Racket performance and
benchmarking, see @secref[#:doc '(lib
"scribblings/guide/guide.scrbl")]{performance}.}

Typed Racket provides a type-driven optimizer that rewrites well-typed
programs to potentially make them faster.

@section{Turning the optimizer off}

Typed Racket's optimizer is turned on by default. If you want to
deactivate it (for debugging, for instance), you must add the
@as-index{@racket[#:no-optimize]} keyword when specifying the language of your
program:

@racketmod[typed/racket #:no-optimize]

The optimizer is also disabled when executing a typed racket program in
a sandbox (see @secref["Sandboxed_Evaluation" #:doc
'(lib "scribblings/reference/reference.scrbl")]) and when the environment
variable @envvar{PLT_TR_NO_OPTIMIZE} is set (to any value).

@section{Getting the most out of the optimizer}
Typed Racket's optimizer can improve the performance of various common
Racket idioms. However, it does a better job on some idioms than on
others. By writing your programs using the right idioms, you can help
the optimizer help you.

To best take advantage of the Typed Racket optimizer, consult
@other-doc['(lib "optimization-coach/scribblings/optimization-coach.scrbl")
#:indirect "Optimization Coach"]{}.

The Typed Racket optimizer logs events with the topic
@indexed-racket['TR-optimizer].
See @Secref["logging" #:doc '(lib "scribblings/reference/reference.scrbl")]
to learn how to receive these log events.


@subsection{Numeric types}
Being type-driven, the optimizer makes most of its decisions based on
the types you assigned to your data. As such, you can improve the
optimizer's usefulness by writing informative types.

For example, the following programs both typecheck:
@racketblock[(define (f [x : Real])  : Real  (+ x 2.5))
             (f 3.5)]
@racketblock[(define (f [x : Float]) : Float (+ x 2.5))
             (f 3.5)]

However, the second one uses more informative types: the
@racket[Float] type includes only 64-bit floating-point numbers
whereas the
@racket[Real] type includes both exact and
@tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key
"inexact numbers"]{inexact}
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{real numbers}
and the @racket[Inexact-Real] type includes both 32- and 64-bit
floating-point numbers.
Typed Racket's optimizer can optimize the latter program to use
@tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key
"inexact numbers"]{float}
-specific operations whereas it cannot do anything with the
former program.

Thus, to get the most of Typed Racket's optimizer, you should use the
@racket[Float] type when possible. For similar reasons, you should use
floating-point literals instead of exact literals when doing
floating-point computations.

When mixing floating-point numbers and exact reals in arithmetic
operations, the result is not necessarily a @racket[Float]. For
instance, the result of @racket[(* 2.0 0)] is @racket[0] which is not
a @racket[Float]. This can result in missed optimizations. To prevent
this, when mixing floating-point numbers and exact reals, coerce exact
reals to floating-point numbers using @racket[real->double-flonum].
This is not necessary when using @racket[+] or @racket[-]. When mixing
floating-point numbers of different precisions, results use the
highest precision possible.

On a similar note, the @racket[Float-Complex] type is preferable to
the @racket[Complex] type for the same reason. Typed Racket can keep
float
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{complex numbers}
unboxed; as such, programs using
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{complex numbers}
can have better performance than equivalent programs that
represent
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{complex numbers}
as two
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{real numbers}.
As with floating-point literals, float
@tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key
"complex numbers"]{complex}
literals (such as @racket[1.0+1.0i]) should be preferred over exact
@tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key
"complex numbers"]{complex}
literals (such as @racket[1+1i]). Note that both parts of a literal must be
present and
@tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key
"inexact numbers"]{inexact}
for the literal to be of type
@racket[Float-Complex]; @racket[0.0+1.0i] is of type
@racket[Float-Complex] but @racket[+1.0i] is not.
To get the most of
Typed Racket's optimizer, you should also favor rectangular
coordinates over polar coordinates.

@subsection{Lists}
Typed Racket handles potentially empty lists and lists that are known
to be non-empty differently: when taking the @racket[car] or the
@racket[cdr] of a list Typed Racket knows is non-empty, it can skip
the check for the empty list that is usually done when calling
@racket[car] and @racket[cdr].

@racketblock[
(define (sum [l : (Listof Integer)]) : Integer
  (if (null? l)
      0
      (+ (car l) (sum (cdr l)))))
]

In this example, Typed Racket knows that if we reach the else branch,
@racket[l] is not empty. The checks associated with @racket[car] and
@racket[cdr] would be redundant and are eliminated.

In addition to explicitly checking for the empty list using
@racket[null?], you can inform Typed Racket that a list is non-empty
by using the known-length list type constructor; if your data is
stored in lists of fixed length, you can use the @racket[List] type
constructors.

For instance, the type of a list of two @racket[Integer]s can be
written either as:
@racketblock[(define-type List-2-Ints (Listof Integer))]
or as the more precise:
@racketblock[(define-type List-2-Ints (List Integer Integer))]

Using the second definition, all @racket[car] and @racket[cdr]-related
checks can be eliminated in this function:
@racketblock[
(define (sum2 [l : List-2-Ints]) : Integer
  (+ (car l) (car (cdr l))))
]

@subsection{Vectors}

In addition to known-length lists, Typed Racket supports known-length
vectors through the @racket[Vector] type constructor. Known-length
vector access using constant indices can be optimized in a similar
fashion as @racket[car] and @racket[cdr].

@#reader scribble/comment-reader (racketblock
;; #(color r g b)
(define-type Color (Vector String Integer Integer Integer))
(define x : Color (vector "red" 255 0 0))
(vector-ref x 0) ; good
(define color-name 0)
(vector-ref x color-name) ; good
(vector-ref x (* 0 10)) ; bad
)

In many such cases, however, @seclink[#:doc '(lib
"scribblings/guide/guide.scrbl") "define-struct"]{structs} are
preferable to vectors. Typed Racket can optimize struct access in all
cases.

@subsection[#:tag "contract-costs"]{Contract boundaries}

When interoperating with untyped code (see @secref{typed-untyped-interaction}),
contracts are installed between typed and untyped modules. Contracts can have
significant overhead, thus typed-untyped boundary crossings should be avoided
in performance-sensitive code.

Typed Racket provides types for most of the bindings provided by @tt{#lang
racket}; using @racket[require/typed] is unnecessary in these cases.

If you suspect that contracts at a typed-untyped boundary may have a
significant cost in your program, you can investigate further using the
@seclink["top" #:doc '(lib "contract-profile/scribblings/contract-profile.scrbl")
               #:indirect? #t
         "contract profiler"].

If the contract profiler is not already installed, the following command
will install it:

@commandline{raco pkg install contract-profile}
