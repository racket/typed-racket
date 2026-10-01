#lang scribble/manual

@begin[(require "../utils.rkt")
       (require scribble/example)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@(define the-top-eval (make-base-eval))
@(the-top-eval '(require (except-in typed/racket #%module-begin)))

@title{Exploring Types}

In addition to printing a summary of the types of REPL results, Typed Racket
provides interactive utilities to explore and query types.
The following bindings are only available at the Typed Racket REPL.

@defform[(:type maybe-verbose t)
         #:grammar ([maybe-verbose (code:line)
                                   (code:line #:verbose)])]{
  Prints the type @racket[_t]. If @racket[_t] is a type alias
  (e.g., @racket[Number]), then it will be expanded to its representation
  when printing. Any further type aliases in the type named by @racket[_t]
  will remain unexpanded.

  If @racket[#:verbose] is provided, all type aliases are expanded
  in the printed type and latent propositions and objects are printed.

  @examples[#:eval the-top-eval
    ;; I'm not sure why, but the :type examples below don't work
    ;; without the #%top-interaction in the first example
    (eval:alts (:type Number) (#%top-interaction . (:type Number)))
    (:type Real)
    (:type #:verbose Number)
  ]
}

@defform[(:print-type maybe-verbose e)
         #:grammar ([maybe-verbose (code:line)
                                   (code:line #:verbose)])]{
Prints the type of @racket[_e], which must be an expression. This prints the
whole type, which can sometimes be quite large. If @racket[#:verbose] is
provided, all type aliases are expanded in the printed type and latent
propositions and objects are printed.

@examples[#:eval the-top-eval
  (:print-type (+ 1 2))
  (:print-type map)
]

When a type error message compares an expected type with the given one, it
prints the latent propositions and objects of both whenever the expected type
has some that would otherwise be omitted. To print them in every type that a
module's error messages show, use the @racket[#:print-propositions] language
option:

@racketmod[typed/racket #:print-propositions]
}

@defform[(:query-type/args f t ...)]{Given a function @racket[f] and argument
types @racket[t], shows the result type of @racket[f].

@examples[#:eval the-top-eval
  (:query-type/args + Integer Number)
]
}

@defform[(:query-type/result f t)]{Given a function @racket[f] and a desired
return type @racket[t], shows the arguments types @racket[f] should be given to
return a value of type @racket[t].

@examples[#:eval the-top-eval
  (:query-type/result + Integer)
  (:query-type/result + Float)
]
}

@defform[(:kind e)]{Prints the kind of a well-kinded type-level expression
@racket[e]. When @racket[e] is a type, it prints @racket[*]. When @racket[e] is
a type constructor, @racket[->] following the open parenthesis in the printed
result indicates @racket[e] is productive and @racket[-o] indicates otherwise.

@examples[#:eval the-top-eval
  (:kind Integer)
  (:kind Listof)
  (:kind Pairof)
  (:kind U)
]

@history[#:added "1.15"]
}

@close-eval[the-top-eval]
