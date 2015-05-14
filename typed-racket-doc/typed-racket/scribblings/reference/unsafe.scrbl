#lang scribble/manual

@begin[(require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@title{Unsafe Typed Racket operations}

@defmodule[typed/racket/unsafe]

@bold{Warning}: the operations documented in this section are @emph{unsafe},
meaning that they can circumvent the invariants of the type system. Unless the
@racket[#:no-optimize] language option is used, this may result in unpredictable
behavior and may even crash Typed Racket.

@defform[(require/typed/unsafe m rt-clause ...)]{
  This form requires identifiers from the module @racket[m] with the same
  import specifications as @racket[require/typed].

  Unlike @racket[require/typed], this form is unsafe and will not generate
  contracts that correspond to the specified types to check that the values
  actually match their types.
}

@defform[(provide/unsafe provide-spec ...)]{
  This form declares exports from a module with the same syntax as
  the @racket[provide] form.

  Unlike @racket[provide], this form is unsafe and Typed Racket will not generate
  any contracts that correspond to the specified types. This means that uses of the
  exports in other modules may circumvent the type system's invariants.
}
