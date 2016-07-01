#lang scribble/manual

@begin[(require "utils.rkt" (for-label (only-meta-in 0 typed/racket)))]

@title[#:tag "top"]{The Typed Racket Guide}

@author[@author+email["Sam Tobin-Hochstadt" "samth@racket-lang.org"]
        @author+email["Vincent St-Amour" "stamourv@racket-lang.org"]
        @author+email["Eric Dobson" "endobson@racket-lang.org"]
        @author+email["Asumu Takikawa" "asumu@racket-lang.org"]
        ]

@section-index["typechecking" "typechecker" "typecheck"]

Typed Racket is Racket's gradually-typed sister language which
lets you add statically-checked type annotations to your programs.
This guide is intended for programmers familiar
with Racket.  For an introduction to Racket, see @(other-manual '(lib "scribblings/guide/guide.scrbl")).

For the precise details, also see
@other-doc['(lib "typed-racket/scribblings/ts-reference.scrbl")].

@local-table-of-contents[]

@include-section["guide/quick.scrbl"]
@include-section["guide/begin.scrbl"]
@include-section["guide/more.scrbl"]
@include-section["guide/types.scrbl"]
@include-section["guide/occurrence.scrbl"]
@include-section["guide/typed-untyped-interaction.scrbl"]
@include-section["guide/optimization.scrbl"]
@include-section["guide/caveats.scrbl"]

@;@section{How the Type System Works}

@;@section{Integrating with Untyped Code}
