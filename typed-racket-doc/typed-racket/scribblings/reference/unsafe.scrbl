#lang scribble/manual

@(require scribble/eval
          (for-label (only-meta-in 0 [except-in typed/racket for])))

@(define eval (make-base-eval))
@(eval '(require typed/racket/base))

@title{Unsafe Typed Racket operations}

@defmodule[typed/racket/unsafe]

@bold{Warning}: the operations documented in this section are @emph{unsafe},
meaning that they can circumvent the invariants of the type system. Unless the
@racket[#:no-optimize] language option is used, this may result in unpredictable
behavior and may even crash Typed Racket.

@defform[(unsafe-require/typed m rt-clause ...)]{
  This form requires identifiers from the module @racket[m] with the same
  import specifications as @racket[require/typed].

  Unlike @racket[require/typed], this form is unsafe and will not generate
  contracts that correspond to the specified types to check that the values
  actually match their types.

  @examples[#:eval eval
    (require typed/racket/unsafe)
    (code:comment "import with a bad type")
    (unsafe-require/typed racket/base [values (-> String Integer)])
    (code:comment "unchecked call, the result type is wrong")
    (values "foo")
  ]

  @history[#:added "1.3"]
}

@defform[(unsafe-provide provide-spec ...)]{
  This form declares exports from a module with the same syntax as
  the @racket[provide] form.

  Unlike @racket[provide], this form is unsafe and Typed Racket will not generate
  any contracts that correspond to the specified types. This means that uses of the
  exports in other modules may circumvent the type system's invariants.

  @examples[#:eval eval
    (module t typed/racket/base
      (require typed/racket/unsafe)
      (: f (-> Integer Integer))
      (define (f x) (add1 x))
      (code:comment "unsafe export, does not install checks")
      (unsafe-provide f))

    (module u racket/base
      (require 't)
      (code:comment "bad call that's unchecked")
      (f "foo"))

    (require 'u)
  ]

  @history[#:added "1.3"]
}

@close-eval[eval]
