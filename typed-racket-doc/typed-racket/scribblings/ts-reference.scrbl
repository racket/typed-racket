#lang scribble/manual

@title[#:tag "top"]{The Typed Racket Reference}

@author[@author+email["Sam Tobin-Hochstadt" "samth@racket-lang.org"]
        @author+email["Vincent St-Amour" "stamourv@racket-lang.org"]
        @author+email["Eric Dobson" "endobson@racket-lang.org"]
        @author+email["Asumu Takikawa" "asumu@racket-lang.org"]
        ]

This manual describes the Typed Racket language, a sister language
of Racket with a static type-checker. The types, special forms, and
other tools provided by Typed Racket are documented here.

For a friendly introduction, see the companion manual
@other-doc['(lib "typed-racket/scribblings/ts-guide.scrbl")].

For more information on the motivation and design of Typed Racket,
 see @cite{POPL-2008} and @cite{SNAPL-2017}.

For more information on occurrence typing,
 see @cite{ICFP-2010}.
For applications and extensions of occurrence typing,
 see @cite{PLDI-2016} and @cite{POPL-2017}.

For more information on other aspects of the Typed Racket type system,
 see @cite{ESOP-2009}, @cite{OOPSLA-2012}, and @cite{ECOOP-2015}.

@(defmodulelang* (typed/racket/base typed/racket)
                 #:use-sources
                    (typed-racket/typed-racket
                     typed-racket/base-env/prims
                     typed-racket/base-env/extra-procs
                     typed-racket/base-env/base-types
                     typed-racket/base-env/base-types-extra))

@local-table-of-contents[]

@include-section["reference/types.scrbl"]
@include-section["reference/special-forms.scrbl"]
@include-section["reference/libraries.scrbl"]
@include-section["reference/typed-classes.scrbl"]
@include-section["reference/typed-units.scrbl"]
@include-section["reference/utilities.scrbl"]
@include-section["reference/exploring-types.scrbl"]
@include-section["reference/no-check.scrbl"]
@include-section["reference/typed-regions.scrbl"]
@include-section["reference/optimization.scrbl"]
@include-section["reference/unsafe.scrbl"]
@include-section["reference/legacy.scrbl"]
@include-section["reference/compatibility-languages.scrbl"]
@include-section["reference/experimental.scrbl"]

@(bibliography
   (bib-entry #:key "POPL-2008"
              #:author "Sam Tobin-Hochstadt and Matthias Felleisen"
              #:title "The Design and Implementation of Typed Scheme"
              #:location "Symposium on Principles of Programming Languages"
              #:date "2008")
   (bib-entry #:key "ICFP-2010"
              #:author "Sam Tobin-Hochstadt and Matthias Felleisen"
              #:title "Logical Types for Untyped Languages"
              #:location "International Conference on Functional Programming"
              #:date "2010")
   (bib-entry #:key "ESOP-2009"
              #:author "T. Stephen Strickland, Sam Tobin-Hochstadt, and Matthias Felleisen"
              #:title "Practical Variable-Arity Polymorphism"
              #:location "European Symposium on Programming"
              #:date "2009")
   (bib-entry #:key "OOPSLA-2012"
              #:author "Asumu Takikawa, T. Stephen Strickland, Christos Dimoulas, Sam Tobin-Hochstadt, and Matthias Felleisen"
              #:title "Gradual Typing for First-Class Classes"
              #:location "Conference on Object-Oriented Programming, Systems, Languages, and Applications"
              #:date "2012")
   (bib-entry #:key "ESOP-2013"
              #:author "Asumu Takikawa, T. Stephen Strickland, and Sam Tobin-Hochstadt"
              #:title "Constraining Delimited Control with Contracts"
              #:location "European Symposium on Programming"
              #:date "2013")
   (bib-entry #:key "ECOOP-2015"
              #:author "Asumu Takikawa, Daniel Feltey, Earl Dean, Robert Bruce Findler, Matthew Flatt, Sam Tobin-Hochstadt, and Matthias Felleisen"
              #:title "Toward Practical Gradual Typing"
              #:location "European Conference on Object-Oriented Programming"
              #:date "2015")
   (bib-entry #:key "PLDI-2016"
              #:author "Andrew Kent and Sam Tobin-Hochstadt"
              #:title "Occurrence Typing Modulo Theories"
              #:location "Conference on Programming Languages Design and Implementation"
              #:date "2016")
   (bib-entry #:key "POPL-2017"
              #:author "Stephen Chang, Alex Knauth, and Emina Torlak"
              #:title "Symbolic Types for Lenient Symbolic Execution"
              #:location "Symposium on Principles of Programming Languages"
              #:date "2017")
   (bib-entry #:key "SNAPL-2017"
              #:author "Sam Tobin-Hochstadt, Matthias Felleisen, Robert Bruce Findler, Matthew Flatt, Ben Greenman, Andrew M. Kent, Vincent St-Amour, T. Stephen Strickland, and Asumu Takikawa"
              #:title "Migratory Typing: Ten Years Later"
              #:location "Summit oN Advances in Programming Languages"
              #:date "2017"))

@index-section[]
