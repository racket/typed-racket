#lang scribble/manual

@require[(for-label racket/control)]

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
For technical details, refer to the @secref{tr-bibliography}.

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

@(define (bib-note . str*)
   (list (linebreak) (linebreak) str* (linebreak) (linebreak)))

@(define DLS "Dynamic Languages Symposium")
@(define SFP "Workshop on Scheme and Functional Programming")
@(define POPL "Symposium on Principles of Programming Languages")
@(define ESOP "European Symposium on Programming")
@(define ICFP "International Conference on Functional Programming")
@(define PHD-DISSERTATION "Ph.D. dissertation")
@(define PLDI "Conference on Programming Language Design and Implementation")
@(define OOPSLA "Conference on Object-Oriented Programming, Systems, Languages, and Applications")
@(define PADL "International Symposium on Practical Aspects of Declarative Languages")
@(define ECOOP "European Conference on Object-Oriented Programming")
@(define SNAPL "Summit oN Advances in Programming Languages")

@(bibliography #:tag "tr-bibliography"
   (bib-entry #:key "DLS-2006"
              #:author "Sam Tobin-Hochstadt and Matthias Felleisen"
              #:title "Interlanguage Migration: from Scripts to Programs"
              #:location DLS
              #:date "2006"
              #:url "https://www2.ccs.neu.edu/racket/pubs/dls06-tf.pdf"
              #:note @bib-note{
                       Presents the original model for module-level
                       gradual typing.
                       In the model, one typed module may interact with any
                       number of untyped modules.
                       A type soundness theorem guarantees the integrity of all typed
                       code.})
   (bib-entry #:key "SFP-2007"
              #:author "Ryan Culpepper, Sam Tobin-Hochstadt, and Matthew Flatt"
              #:title "Advanced Macrology and the Implementation of Typed Scheme"
              #:location SFP
              #:date "2007"
              #:url "https://www2.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf"
              #:note @bib-note{
                       Describes the key macros that enabled Typed Racket.})
   (bib-entry #:key "POPL-2008"
              #:author "Sam Tobin-Hochstadt and Matthias Felleisen"
              #:title "The Design and Implementation of Typed Scheme"
              #:location POPL
              #:date "2008"
              #:url "https://www2.ccs.neu.edu/racket/pubs/popl08-thf.pdf"
              #:note @bib-note{
                       Contains a model of core Typed Racket (with a simple
                       form of occurrence typing) and an extended discussion
                       about scaling the model to a language.})
   (bib-entry #:key "ESOP-2009"
              #:author "T. Stephen Strickland, Sam Tobin-Hochstadt, and Matthias Felleisen"
              #:title "Practical Variable-Arity Polymorphism"
              #:location ESOP
              #:date "2009"
              #:url "https://www2.ccs.neu.edu/racket/pubs/esop09-sthf.pdf"
              #:note @bib-note{
                       Explains how to type-check a polymorphic function that
                       accepts any number of arguments (such as @racket[map]).})
   (bib-entry #:key "ICFP-2010"
              #:author "Sam Tobin-Hochstadt and Matthias Felleisen"
              #:title "Logical Types for Untyped Languages"
              #:location ICFP
              #:date "2010"
              #:url "https://www2.ccs.neu.edu/racket/pubs/icfp10-thf.pdf"
              #:note @bib-note{
                       Presents a compositionas occurrence typing system and
                       comments on its implementation in Typed Racket.})
   (bib-entry #:key "Tobin-Hochstadt"
              #:author "Sam Tobin-Hochstadt"
              #:title "Typed Scheme: From Scripts to Programs"
              #:location PHD-DISSERTATION
              #:date "2010"
              #:url "https://www2.ccs.neu.edu/racket/pubs/dissertation-tobin-hochstadt.pdf")
   (bib-entry #:key "PLDI-2011"
              #:author "Sam Tobin-Hochstadt, Vincent St-Amour, Ryan Culpepper, Matthew Flatt, and Matthias Felleisen"
              #:title "Languages as Libraries"
              #:location PLDI
              #:date "2011"
              #:url "https://www2.ccs.neu.edu/racket/pubs/pldi11-thacff.pdf"
              #:note @bib-note{
                       Motivates the use of macros to define a language and
                       summarizes the Typed Racket type checker and optimizer.})
   (bib-entry #:key "OOPSLA-2012"
              #:author "Asumu Takikawa, T. Stephen Strickland, Christos Dimoulas, Sam Tobin-Hochstadt, and Matthias Felleisen"
              #:title "Gradual Typing for First-Class Classes"
              #:location OOPSLA
              #:date "2012"
              #:url "https://www2.ccs.neu.edu/racket/pubs/oopsla12-tsdthf.pdf"
              #:note @bib-note{
                       Presents a model of typed classes that can interact with
                       untyped classes through method calls, inheritance, and
                       mixins.})
   (bib-entry #:key "PADL-2012"
              #:author "Vincent St-Amour, Sam Tobin-Hochstadt, Matthew Flatt, and Matthias Felleisen"
              #:title "Typing the Numeric Tower"
              #:location PADL
              #:date "2012"
              #:url "https://www2.ccs.neu.edu/racket/pubs/padl12-stff.pdf"
              #:note @bib-note{
                       Motivates the built-in types for numbers
                       and numeric primitives.})
   (bib-entry #:key "ESOP-2013"
              #:author "Asumu Takikawa, T. Stephen Strickland, and Sam Tobin-Hochstadt"
              #:title "Constraining Delimited Control with Contracts"
              #:location ESOP
              #:date "2013"
              #:url "https://www2.ccs.neu.edu/racket/pubs/esop13-tsth.pdf"
              #:note @bib-note{
                       Shows how to type check the @racket[%] and @racket[fcontrol]
                       operators in the presence of continuation marks.})
   (bib-entry #:key "ECOOP-2015"
              #:author "Asumu Takikawa, Daniel Feltey, Earl Dean, Robert Bruce Findler, Matthew Flatt, Sam Tobin-Hochstadt, and Matthias Felleisen"
              #:title "Toward Practical Gradual Typing"
              #:location ECOOP
              #:date "2015"
              #:url "https://www2.ccs.neu.edu/racket/pubs/ecoop2015-takikawa-et-al.pdf"
              #:note @bib-note{
                       Presents an implementation, experience report, and
                       performance evaluation for gradually-typed first-class
                       classes.})
   (bib-entry #:key "PLDI-2016"
              #:author "Andrew Kent and Sam Tobin-Hochstadt"
              #:title "Occurrence Typing Modulo Theories"
              #:location PLDI
              #:date "2016"
              #:url "https://dl.acm.org/citation.cfm?id=2908091"
              #:note @bib-note{
                       Adds linear integer constraints to Typed Racket's
                       compositional occurrence typing.})
   (bib-entry #:key "Takikawa"
              #:author "Asumu Takikawa"
              #:title "The Design, Implementation, and Evaluation of a Gradual Type System for Dynamic Class Composition"
              #:location PHD-DISSERTATION
              #:date "2016"
              #:url "https://www2.ccs.neu.edu/racket/pubs/dissertation-takikawa.pdf")
   (bib-entry #:key "POPL-2017"
              #:author "Stephen Chang, Alex Knauth, and Emina Torlak"
              #:title "Symbolic Types for Lenient Symbolic Execution"
              #:location POPL
              #:date "2017"
              #:url "https://www2.ccs.neu.edu/racket/pubs/popl18-ckt.pdf"
              #:note @bib-note{
                       Presents a typed version of @hyperlink["https://emina.github.io/rosette"]{Rosette}
                       that distinguishes between concrete and symbolic values.
                       The type system supports occurrence typing.})
   (bib-entry #:key "SNAPL-2017"
              #:author "Sam Tobin-Hochstadt, Matthias Felleisen, Robert Bruce Findler, Matthew Flatt, Ben Greenman, Andrew M. Kent, Vincent St-Amour, T. Stephen Strickland, and Asumu Takikawa"
              #:title "Migratory Typing: Ten Years Later"
              #:location SNAPL
              #:date "2017"
              #:url "https://www2.ccs.neu.edu/racket/pubs/typed-racket.pdf"
              #:note @bib-note{
                       Reflects on origins and successes; looks ahead to
                       current and future challenges.}))

@index-section[]
