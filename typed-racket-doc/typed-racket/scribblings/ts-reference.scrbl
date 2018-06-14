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
 see @cite{POPL-2008} (original design),
 @cite{DLS-2006} (early design),
 and @cite{SNAPL-2017} (retrospective).

For more information on occurrence typing,
 see @cite{ICFP-2010}.
For extensions and applications of occurrence typing,
 see @cite{PLDI-2016} (extension: adding linear integer refinements)
 and @cite{POPL-2017} (application: lenient symbolic execution).

For more information on other aspects of the Typed Racket type system,
 see @cite{ESOP-2009} (variable-arity polymorphism),
 @cite{OOPSLA-2012} (design of class/object types),
 @cite{PADL-2012} (numeric types),
 and @cite{ECOOP-2015} (implementation of class/object types).

For more information on the implementation of Typed Racket,
 see @cite{PLDI-2011} and @cite{SFP-2007} (macros).

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
   (bib-entry #:key "DLS-2006"
              #:author "Sam Tobin-Hochstadt and Matthias Felleisen"
              #:title "Interlanguage Migration: from Scripts to Programs"
              #:location "Dynamic Languages Symposium"
              #:date "2006"
              #:url "https://www2.ccs.neu.edu/racket/pubs/dls06-tf.pdf")
   (bib-entry #:key "SFP-2007"
              #:author "Ryan Culpepper, Sam Tobin-Hochstadt, and Matthew Flatt"
              #:title "Advanced Macrology and the Implementation of Typed Scheme"
              #:location "Workshop on Scheme and Functional Programming"
              #:date "2007"
              #:url "https://www2.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf")
   (bib-entry #:key "POPL-2008"
              #:author "Sam Tobin-Hochstadt and Matthias Felleisen"
              #:title "The Design and Implementation of Typed Scheme"
              #:location "Symposium on Principles of Programming Languages"
              #:date "2008"
              #:url "https://www2.ccs.neu.edu/racket/pubs/popl08-thf.pdf")
   (bib-entry #:key "ESOP-2009"
              #:author "T. Stephen Strickland, Sam Tobin-Hochstadt, and Matthias Felleisen"
              #:title "Practical Variable-Arity Polymorphism"
              #:location "European Symposium on Programming"
              #:date "2009"
              #:url "https://www2.ccs.neu.edu/racket/pubs/esop09-sthf.pdf")
   (bib-entry #:key "ICFP-2010"
              #:author "Sam Tobin-Hochstadt and Matthias Felleisen"
              #:title "Logical Types for Untyped Languages"
              #:location "International Conference on Functional Programming"
              #:date "2010"
              #:url "https://www2.ccs.neu.edu/racket/pubs/icfp10-thf.pdf")
   (bib-entry #:key "Tobin-Hochstadt"
              #:author "Sam Tobin-Hochstadt"
              #:title "Typed Scheme: From Scripts to Programs"
              #:location "Ph.D. dissertation"
              #:date "2010"
              #:url "https://www2.ccs.neu.edu/racket/pubs/dissertation-tobin-hochstadt.pdf")
   (bib-entry #:key "PLDI-2011"
              #:author "Sam Tobin-Hochstadt, Vincent St-Amour, Ryan Culpepper, Matthew Flatt, and Matthias Felleisen"
              #:title "Languages as Libraries"
              #:location "Conference on Programming Language Design and Implementation"
              #:date "2011"
              #:url "https://www2.ccs.neu.edu/racket/pubs/pldi11-thacff.pdf")
   (bib-entry #:key "OOPSLA-2012"
              #:author "Asumu Takikawa, T. Stephen Strickland, Christos Dimoulas, Sam Tobin-Hochstadt, and Matthias Felleisen"
              #:title "Gradual Typing for First-Class Classes"
              #:location "Conference on Object-Oriented Programming, Systems, Languages, and Applications"
              #:date "2012"
              #:url "https://www2.ccs.neu.edu/racket/pubs/oopsla12-tsdthf.pdf")
   (bib-entry #:key "PADL-2012"
              #:author "Vincent St-Amour, Sam Tobin-Hochstadt, Matthew Flatt, and Matthias Felleisen"
              #:title "Typing the Numeric Tower"
              #:location "International Symposium on Practical Aspects of Declarative Languages"
              #:date "2012"
              #:url "https://www2.ccs.neu.edu/racket/pubs/padl12-stff.pdf")
   (bib-entry #:key "ESOP-2013"
              #:author "Asumu Takikawa, T. Stephen Strickland, and Sam Tobin-Hochstadt"
              #:title "Constraining Delimited Control with Contracts"
              #:location "European Symposium on Programming"
              #:date "2013"
              #:url "https://www2.ccs.neu.edu/racket/pubs/esop13-tsth.pdf")
   (bib-entry #:key "ECOOP-2015"
              #:author "Asumu Takikawa, Daniel Feltey, Earl Dean, Robert Bruce Findler, Matthew Flatt, Sam Tobin-Hochstadt, and Matthias Felleisen"
              #:title "Toward Practical Gradual Typing"
              #:location "European Conference on Object-Oriented Programming"
              #:date "2015"
              #:url "https://www2.ccs.neu.edu/racket/pubs/ecoop2015-takikawa-et-al.pdf")
   (bib-entry #:key "PLDI-2016"
              #:author "Andrew Kent and Sam Tobin-Hochstadt"
              #:title "Occurrence Typing Modulo Theories"
              #:location "Conference on Programming Languages Design and Implementation"
              #:date "2016"
              #:url "https://dl.acm.org/citation.cfm?id=2908091")
   (bib-entry #:key "Takikawa"
              #:author "Asumu Takikawa"
              #:title "The Design, Implementation, and Evaluation of a Gradual Type System for Dynamic Class Composition"
              #:location "Ph.D. dissertation"
              #:date "2016"
              #:url "https://www2.ccs.neu.edu/racket/pubs/dissertation-takikawa.pdf")
   (bib-entry #:key "POPL-2017"
              #:author "Stephen Chang, Alex Knauth, and Emina Torlak"
              #:title "Symbolic Types for Lenient Symbolic Execution"
              #:location "Symposium on Principles of Programming Languages"
              #:date "2017"
              #:url "https://www2.ccs.neu.edu/racket/pubs/popl18-ckt.pdf")
   (bib-entry #:key "SNAPL-2017"
              #:author "Sam Tobin-Hochstadt, Matthias Felleisen, Robert Bruce Findler, Matthew Flatt, Ben Greenman, Andrew M. Kent, Vincent St-Amour, T. Stephen Strickland, and Asumu Takikawa"
              #:title "Migratory Typing: Ten Years Later"
              #:location "Summit oN Advances in Programming Languages"
              #:date "2017"
              #:url "https://www2.ccs.neu.edu/racket/pubs/typed-racket.pdf"))

@index-section[]
