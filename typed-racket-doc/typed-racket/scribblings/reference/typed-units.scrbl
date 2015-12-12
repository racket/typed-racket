#lang scribble/manual

@begin[(require "../utils.rkt" scribble/example racket/sandbox)
       (require (for-label (only-meta-in 0 [except-in typed/racket for]))
       (for-label (only-in racket/unit tag unit/c)))]

@(define the-eval (make-base-eval))
@(the-eval '(require (except-in typed/racket #%top-interaction #%module-begin)))
@(define the-top-eval (make-base-eval))
@(the-top-eval '(require (except-in typed/racket #%module-begin)))

@(define-syntax-rule (ex . args)
   (examples #:eval the-top-eval . args))

@title{Typed Units}

@bold{Warning}: the features described in this section are experimental
and may not work correctly. Some of the features may change by
the next release.

Typed Racket provides support for modular programming with the units
and signatures provided by the @racketmodname[racket/unit] library.

@section[#:tag "unit-forms"]{Special forms}

@defmodule[typed/racket/unit]

The special forms below are provided by the @racketmodname[typed/racket/unit]
and @racketmodname[typed/racket] modules, but not by
@racketmodname[typed/racket/base]. The @racketmodname[typed/racket/unit] module
additionally provides all other bindings from @racketmodname[racket/unit].

@;; This trick is borrowed from the Typed Class reference to link to
@;; the identifiers in racket/unit
@(module id-holder racket/base
  (require scribble/manual (for-label racket/unit))
  (provide (all-defined-out))
  (define ut:define-signature (racket define-signature))
  (define ut:unit (racket unit))
  (define ut:invoke-unit (racket invoke-unit))
  (define ut:define-values/invoke-unit (racket define-values/invoke-unit))
  (define ut:compound-unit (racket compound-unit))
  (define ut:define-unit (racket define-unit))
  (define ut:compound-unit/infer (racket compound-unit/infer))
  (define ut:define-compound-unit (racket define-compound-unit))
  (define ut:define-compound-unit/infer (racket define-compound-unit/infer))
  (define ut:invoke-unit/infer (racket invoke-unit/infer))
  (define ut:define-values/invoke-unit/infer (racket define-values/invoke-unit/infer))
  (define ut:unit-from-context (racket unit-from-context))
  (define ut:define-unit-from-context (racket define-unit-from-context))
  (define ut:define-values-for-export (racket define-values-for-export))
  (define ut:define-values (racket define-values))
  (define ut:open (racket open))
  (define ut:define-syntaxes (racket define-syntaxes))
  (define ut:define-unit-binding (racket define-unit-binding))
  (define ut:unit/s (racket unit/s))
  (define ut:unit/new-import-export (racket unit/new-import-export)))
@(require 'id-holder)


@defform[
#:literals (extends :) 
(define-signature id extension-decl
  (sig-elem ...))
#:grammar
  ([extension-decl 
    code:blank
    (code:line extends sig-id)]

   [sig-elem  [id : type]])]{

Binds an identifier to a signature and registers the identifier in the signature
environment with the specified type bindings. Sigantures in Typed Racket allow
only specifications of variables and their types. Variable and syntax definitions
are not allowed in the @racket[define-signature] form. This is only a limitation
of the @racket[define-signature] form in Typed Racket.

As in untyped Racket, the @racket[extends] clause includes all elements of
extended signature and any implementation of the new signature can be used
as an implementation of the extended signature.}

@defform[
#:literals (import export prefix rename only except init-depend)
(unit 
  (import sig-spec ...)
  (export sig-spec ...)
  init-depends-decl
  unit-body-expr-or-defn
  ...)
#:grammar ([sig-spec
 	    sig-id  
	    (prefix id sig-spec)
  	    (rename sig-spec (id id) ...)
	    (only sig-spec id ...)
	    (except sig-spec id ...)]

 	    [init-depends-decl
  	    code:blank
  	    (init-depend sig-id ...)])]{
The typed version of the Racket @ut:unit form. Unit expressions in Typed Racket
do not support tagged signatures with the @racket[tag] keyword.}

@defform*[
#:literals (import)
[(invoke-unit unit-expr)
 (invoke-unit unit-expr (import sig-spec ...))]]{
The typed version of the Racket @ut:invoke-unit form.}

@defform[
#:literals (import export)
(define-values/invoke-unit unit-expr
  (import def-sig-spec ...)
  (export def-sig-spec ...))
#:grammar ([def-sig-spec
	    sig-id
	    (prefix id def-sig-spec)
	    (rename def-sig-spec (id id) ...)])]{
The typed version of the Racket @ut:define-values/invoke-unit form. In Typed
Racket @racket[define-values/invoke-unit] is only allowed at the top-level
of a module.}

@defform[
#:literals (: import export link tag)
(compound-unit
  (import link-binding ...)
  (export link-id ...)
  (link linkage-decl ...))
#:grammar ([link-binding
	    (link-id : sig-id)]

	   [linkage-decl
	     ((link-binding ...) unit-expr link-id ...)])]{
The typed version of the Racket @ut:compound-unit form.}

@defform[
#:literals (import export)
(define-unit unit-id
  (import sig-spec ...)
  (export sig-spec ...)
  init-depends-decl
  unit-body-expr-or-defn
  ...)]{
The typed version of the Racket @ut:define-unit form.}

@defform[
#:literals (import export link :)
(compound-unit/infer
  (import infer-link-import ...)
  (export infer-link-export ...)
  (link infer-linkage-decl ...))
#:grammar ([infer-link-import
	    sig-id
	    (link-id : sig-id)]

	   [infer-link-export
	     link-id
	     sig-id]

	   [infer-linkage-decl
	     ((link-binding ...) unit-id
	     		    	 tagged-link-id ...)
	     unit-id])]{
The typed version of the Racket @ut:compound-unit/infer form.}

@defform[
#:literals (import export link)
(define-compound-unit id
  (import link-binding ...)
  (export link-id ...)
  (link linkage-decl ...))]{
The typed version of the Racket @ut:define-compound-unit form.}

@defform[
#:literals (import export link)
(define-compound-unit/infer id
  (import link-binding ...)
  (export infer-link-export ...)
  (link infer-linkage-decl ...))]{
The typed version of the Racket @ut:define-compound-unit/infer form.}

@defform[
#:literals (link)
(invoke-unit/infer unit-spec)
#:grammar ([unit-spec 
	    unit-id
	    (link link-unit-id ...)])]{
The typed version of the Racket @ut:invoke-unit/infer form.}

@defform[
#:literals (export link)
(define-values/invoke-unit/infer maybe-exports unit-spec)
#:grammar ([maybe-exports
	    code:blank
	    (export sig-sepc ...)]
	   [unit-spec
	     unit-id
	     (link link-unit-id ...)])]{
The typed version of the Racket @ut:define-values/invoke-unit/infer form. Like
the @racket[define-values/invoke-unit] form above, this form is only allowed at
the toplevel of a module.}

@defform[
(unit-from-context sig-spec)]{
The typed version of the Racket @ut:unit-from-context form.}

@defform[
(define-unit-from-context id sig-spec)]{
The typed version of the Racket @ut:define-unit-from-context form.}


@section[#:tag "unit-types"]{Types}

@defform[
#:literals (import export init-depend Values)
(Unit 
  (import sig-id ...)
  (export sig-id ...)
  optional-init-depend-clause
  optional-body-type-clause)
#:grammar ([optional-init-depend-clause
             code:blank
             (init-depend sig-id ...)]
           [optional-body-type-clause
             code:blank
	     type
	     (Values type ...)])]{
The type of a unit with the given imports, exports, initialization dependencies,
and body type. Omitting the init-depend clause is equivalent to an
@racket[init-depend] clause that contains no signatures. The body type is the
type of the last expression in the unit's body. If a unit contains only
definitions and no expressions its body type is @racket[Void]. Omitting the body
type is equivalent to specifying a body type of @racket[Void].

@ex[(module Unit-Types typed/racket
      (define-signature fact^ ([fact : (-> Natural Natural)]))
      (: use-fact@ (Unit (import fact^)
	       	         (export)
			 Natural))
      (define use-fact@ (unit (import fact^) (export) (fact 5))))]

}

@defidform[UnitTop]{
The supertype of all unit types. Values of this type cannot be linked or invoked.
The primary use of is for the reflective operation @racket[unit?]}

@section[#:tag "unit-typed/untyped-interactions"]{Interacting with Untyped Code}

@defform/subs[#:link-target? #f
#:literals (struct)
(require/typed m rt-clause ...)
([rt-clause [maybe-renamed t]
            [#:struct name ([f : t] ...)
                 struct-option ...]
            [#:struct (name parent) ([f : t] ...)
                 struct-option ...]
            [#:opaque t pred]
	    [#:signature name ([id : t] ...)]]
 [maybe-renamed id
                (orig-id new-id)]
 [struct-option
   (code:line #:constructor-name constructor-id)
   (code:line #:extra-constructor-name constructor-id)])]


The @racket[#:signature] clause of @racket[require/typed] requires the given
signature and registers it in the signature environment with the specified
bindings. Unlike other identifiers required with @racket[require/typed], signatures
are not protected by contracts.
@margin-note{Signatures are not runtime values and therefore do not need to be protected by contracts.}

@ex[
(module UNTYPED-1 racket
  (provide a^)  
  (define-signature a^ (a)))

(module TYPED-1 typed/racket
  (require/typed 'UNTYPED-1
                 [#:signature a^ ([a : Integer])])
  (unit (import a^) (export) (add1 a)))]


Typed Racket will infer whether the named signature @racket[extends]
another signature. It is an error to require a signature that extends a signature
not present in the signature environment.

@ex[
(module UNTYPED-2 racket
  (provide a-sub^)
  (define-signature a^ (a1))
  (define-signature a-sub^ extends a^ (a2)))

(eval:error
 (module TYPED-2 typed/racket
   (require/typed 'UNTYPED-2
                  [#:signature a-sub^
                    ([a1 : Integer]
                     [a2 : String])])))]


Requiring a signature from an untyped module that contains variable definitions is an error
in Typed Racket.

@ex[
(module UNTYPED racket
  (provide bad^)
  (define-signature bad^ (bad (define-values (bad-ref) (car bad)))))

(eval:error
 (module TYPED typed/racket
   (require/typed 'UNTYPED
                  [#:signature bad^
                    ([bad : (Pairof Integer Integer)]
                     [bad-ref : Integer])])))]








@section{Limitations}

@subsection{Signature Forms}
Unlike Racket's @ut:define-signature form, in Typed Racket
@racket[define-signature] only supports one kind of signature element that
specifies the types of variables in the signature. In particular Typed Racket's
@racket[define-signature] form does not support uses of @ut:define-syntaxes,
@ut:define-values, or @ut:define-values-for-export . Requiring an untyped
signature that contains definitions in a typed module will result in an error.

@ex[(module UNTYPED racket
      (provide bad^)
      (define-signature bad^ ((define-values (bad) 13))))
    (eval:error
     (module TYPED typed/racket
       (require/typed 'UNTYPED
                      [#:signature bad^ ([bad : Integer])])))]

@subsection{Contracts and Unit Static Information}
Unit values that flow between typed and untyped contexts are wrapped in
@racket[unit/c] contracts to guard the unit's imports, exports, and result upon
invocation. When identifers that are additionally bound to static information
about a unit, such as those defined by @racket[define-unit], flow between typed
and untyped contexts contract application can result the static information
becoming inaccessible.

@ex[
(module UNTYPED racket
  (provide u@)
  (define-unit u@ (import) (export) "Hello!"))
(eval:error
 (module TYPED typed/racket
   (require/typed 'UNTYPED
                  [u@ (Unit (import) (export) String)])
   (invoke-unit/infer u@)))]

When an identifier bound to static unit information flows from a typed module to
an untyped module, however, the situation is worse. Because unit static
information is bound to an identifier as a macro definition, any use of the
typed unit is disallowed in untyped contexts.

@ex[
(module TYPED typed/racket
  (provide u@)
  (define-unit u@ (import) (export) "Hello!"))
(eval:error
 (module UNTYPED racket
   (require 'TYPED)
   u@))]

@subsection{Signatures and Internal Definition Contexts}
Typed Racket's @racket[define-signature] form is allowed in both top-level and
internal definition contexts. As the following example shows, defining
signatures in internal definiition contexts can be problematic.

@ex[
(eval:error
 (module TYPED typed/racket
   (define-signature a^ ())
   (define u@
     (let ()
       (define-signature a^ ())
       (unit (import a^) (export) (init-depend a^) 5)))
   (invoke-unit u@ (import a^))))]

  Even though the unit imports a signature named @racket[a^], the @racket[a^]
provided for the import refers to the top-level @racket[a^] signature and the
type system prevents invoking the unit. This issue can be avoided by defining
signatures only at the top-level of a module.

@subsection{Tagged Signatures}

Various unit forms in Racket allow for signatures to be tagged to support the
definition of units that import or export the same signature multiple times.
Typed Racket does not support the use of tagged signatures, using the
@racket[tag] keyword, anywhere in the various unit forms described above.

@subsection{Structural Matching and Other Unit Forms}

Typed Racket supports only those unit forms described above. All other bindings
exported by @racketmodname[racket/unit] are not supported in the type system. In
particular, the structural matching forms including @ut:unit/new-import-export
and @ut:unit/s are unsupported.
