#lang scribble/manual

@begin[(require (for-label (only-meta-in 0 typed/racket))
                scribble/example
                "../utils.rkt" (only-in "quick.scrbl" typed-mod))]

@(define the-eval (make-base-eval))
@(the-eval '(require typed/racket))

@title[#:tag "beginning"]{Beginning Typed Racket}

Recall the typed module from @secref["quick"]:

@|typed-mod|

Let us consider each element of this program in turn.

@racketmod[typed/racket]

This specifies that the module is written in the
@racketmodname[typed/racket] language, which is a typed version of the
@racketmodname[racket] language.  Typed versions of other languages
are provided as well; for example, the
@racketmodname[typed/racket/base] language corresponds to
@racketmodname[racket/base].

@examples[#:no-result #:eval the-eval (struct pt ([x : Real] [y : Real]))]

@margin-note{Typed Racket provides modified versions of core Racket forms,
which permit type annotations. Previous versions of Typed Racket provided
these with a @racket[:] suffix, but these are now only included as legacy
forms for backwards compatibility.}
This defines a new structure, named @racket[pt], with two fields,
@racket[x] and @racket[y].  Both fields are specified to have the type
@racket[Real], which corresponds to the @rtech{real numbers}.
 The
@racket[struct] form corresponds to its untyped counterpart from
@racketmodname[racket]---when porting a program from
@racketmodname[racket] to @racketmodname[typed/racket], simply add
type annotations to existing field declarations.

@examples[#:no-result #:eval the-eval (: distance (-> pt pt Real))]

This declares that @racket[distance] has the type @racket[(-> pt pt Real)].
@;{@racket[distance] must be defined at the top-level of the module containing
the declaration.}

The type @racket[(-> pt pt Real)] is a function type, that is, the type
of a procedure.  The input type, or domain, is two arguments of
type @racket[pt], which refers to an instance of the @racket[pt]
structure.  The @racket[->] indicates that this is a function
type. The range type, or output type, is the last element in the
function type, in this case @racket[Real].

If you are familiar with @rtech{contracts}, the notation for function
types is similar to function contract combinators.

@examples[#:no-result #:eval the-eval
(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-y p1))))))
]

This definition is unchanged from the untyped version of the code.
The goal of Typed Racket is to allow almost all definitions to be
typechecked without change.  The typechecker verifies that the body of
the function has the type @racket[Real], under the assumption that
@racket[p1] and @racket[p2] have type @racket[pt], taking these types
from the earlier type declaration.  Since the body does have this type,
the program is accepted.

In the Typed Racket @gtech{REPL}, calling @racket[distance] will
show the result as usual and will also print the result's type:

@examples[#:label #f #:eval the-eval (distance (pt 0 0) (pt 3.1415 2.7172))]

Just evaluating the function name will print the function value and its type,
which can be useful for discovering the types that Typed Racket ascribes to
Racket functions. Alternatively, the @racket[:print-type] command will just
print the type:

@examples[#:label #f #:eval the-eval distance string-length (:print-type string-ref)]

@section{Datatypes and Unions}

Many data structures involve multiple variants.  In Typed Racket, we
represent these using @italic{union types}, written @racket[(U t1 t2 ...)].

@racketmod[
typed/racket
(define-type Tree (U leaf node))
(struct leaf ([val : Number]))
(struct node ([left : Tree] [right : Tree]))

(: tree-height (-> Tree Integer))
(define (tree-height t)
  (cond [(leaf? t) 1]
        [else (max (+ 1 (tree-height (node-left t)))
                   (+ 1 (tree-height (node-right t))))]))

(: tree-sum (-> Tree Number))
(define (tree-sum t)
  (cond [(leaf? t) (leaf-val t)]
        [else (+ (tree-sum (node-left t))
                 (tree-sum (node-right t)))]))
]

In this module, we have defined two new datatypes: @racket[leaf] and
@racket[node].  We've also defined the type name @racket[Tree] to be
@racket[(U node leaf)], which represents a binary tree of numbers.  In
essence, we are saying that the @racket[tree-height] function accepts
a @racket[Tree], which is either a @racket[node] or a @racket[leaf],
and produces a number.

In order to calculate interesting facts about trees, we have to take
them apart and get at their contents.  But since accessors such as
@racket[node-left] require a @racket[node] as input, not a
@racket[Tree], we have to determine which kind of input we
were passed.

For this purpose, we use the predicates that come with each defined
structure.  For example, the @racket[leaf?] predicate distinguishes
@racket[leaf]s from all other Typed Racket values.  Therefore, in the
first branch of the @racket[cond] clause in @racket[tree-sum], we know
that @racket[t] is a @racket[leaf], and therefore we can get its value
with the @racket[leaf-val] function.

In the else clauses of both functions, we know that @racket[t] is not
a @racket[leaf], and since the type of @racket[t] was @racket[Tree] by
process of elimination we can determine that @racket[t] must be a
@racket[node].  Therefore, we can use accessors such as
@racket[node-left] and @racket[node-right] with @racket[t] as input.

The process by which Typed Racket type-checks the bodies of the
@racket[cond] clauses, using information from the predicate checks,
is called @tech{occurrence typing} and is described in detail in
@Secref["occurrence-typing"].

@section{Type Errors}

When Typed Racket detects a type error in the module, it raises an
error before running the program.

@examples[#:eval the-eval
(eval:error (add1 "not a number"))
]

@;{
Typed Racket also attempts to detect more than one error in the module.

@examples[#:eval the-eval
(eval:error (string-append "a string" (add1 "not a number")))
]
}


@close-eval[the-eval]
