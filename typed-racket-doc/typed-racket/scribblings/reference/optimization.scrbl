#lang scribble/manual

@begin[(require "../utils.rkt" scriblib/footnote)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@title{Optimization in Typed Racket}

@note{
See
@secref[#:doc '(lib "typed-racket/scribblings/ts-guide.scrbl")]{optimization}
in the guide for tips to get the most out of the optimizer.
}

Typed Racket provides a type-driven optimizer that rewrites well-typed
programs to potentially make them faster.

Typed Racket's optimizer is turned on by default. If you want to
deactivate it (for debugging, for instance), you must add the
@racket[#:no-optimize] keyword when specifying the language of your
program:

@racketmod[typed/racket #:no-optimize]

The optimizer is also disabled if the environment variable @envvar{PLT_TR_NO_OPTIMIZE} is set (to any value) or if the current code inspector (see
@secref["modprotect" #:doc '(lib "scribblings/reference/reference.scrbl")])
is insufficiently powerful to access @racketmodname[racket/unsafe/ops],
for example when executing in a sandbox (see @secref["Sandboxed_Evaluation"
#:doc '(lib "scribblings/reference/reference.scrbl")]). This prevents untrusted
code from accessing these operations by exploiting errors in the type system.


@section{Contract Optimization}

Typed Racket generates contracts for its exports to protect them against
untyped code.
By default, these contracts do not check that typed code obeys the types.
If you want to generate contracts that check both sides equally (for analysis,
for teaching, etc.) then set the environment variable
@indexed-envvar{PLT_TR_NO_CONTRACT_OPTIMIZE} to any value and recompile.
