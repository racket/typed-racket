#lang typed/racket/base

(provide (all-defined-out))

(define-type XExpr-Attribute
  (List Symbol String))

;; Assumes permissive-xexprs is False
(define-type XML-Content
  (U Cdata Comment Element Entity P-I Pcdata))

(define-type XML-Misc
  (U Comment P-I))

(define-type XExpr
  (U XML-Misc Cdata Positive-Index
     Number String Symbol
     (Pair Symbol (Pair (Listof XExpr-Attribute) (Listof XExpr)))
     (Pair Symbol (Listof XExpr))))

(require/typed/provide xml
  [#:struct location
   ([line   : (U False Exact-Nonnegative-Integer)]
    [char   : (U False Exact-Nonnegative-Integer)]
    [offset : Exact-Nonnegative-Integer])
   #:type-name Location]
  [#:struct source
   ([start : Location]
    [stop  : Location])
   #:type-name Source]
  [#:struct external-dtd
   ([system : String])
   #:type-name External-Dtd]
  [#:struct (external-dtd/public external-dtd)
   ([public : String])
   #:type-name External-Dtd/Public]
  [#:struct (external-dtd/system external-dtd)
   ()
   #:type-name External-Dtd/System]
  [#:struct document-type
   ([name     : Symbol]
    [external : External-Dtd]
    [inlined  : False])
   #:type-name Document-Type]
  [#:struct comment
   ([text : String])
   #:type-name Comment]
  [#:struct (p-i source)
   ([target-name : Symbol]
    [instruction : String])
   #:type-name P-I]
  [#:struct prolog
   ([misc  : (Listof XML-Misc)]
    [dtd   : (U Document-Type False)]
    [misc2 : (Listof XML-Misc)])
   #:type-name Prolog]
  [#:struct document
   ([prolog  : Prolog]
    [element : Element]
    [misc    : (Listof XML-Misc)])
   #:type-name Document]
  [#:struct (element source)
   ([name       : Symbol]
    [attributes : (Listof Attribute)]
    [content    : (Listof XML-Content)])
   #:type-name Element]
  [#:struct (attribute source)
   ([name  : Symbol]
    [value : Any])
   #:type-name Attribute]
  [#:struct (entity source)
   ([text : (U Symbol Positive-Index)])
   #:type-name Entity]
  [#:struct (pcdata source)
   ([string : String])
   #:type-name Pcdata]
  [#:struct (cdata source)
   ([string : String])
   #:type-name Cdata]
  [valid-char?  (-> Any Boolean)]
  [xexpr?  (-> Any Boolean)]
  [read-xml  (->* () (Input-Port) Document)]
  [read-xml/document  (->* () (Input-Port) Document)]
  [read-xml/element  (->* () (Input-Port) Element)]
  [write-xml  (->* (Document) (Output-Port) Void)]
  [write-xml/content  (->* (XML-Content) (Output-Port) Void)]
  [display-xml
   (->* (Document) (Output-Port #:indentation (U 'none 'classic 'peek 'scan)) Void)]
  [display-xml/content
   (->* (XML-Content) (Output-Port #:indentation (U 'none 'classic 'peek 'scan)) Void)]
  [write-xexpr  (->* (XExpr) (Output-Port #:insert-newlines? Any) Void)]
  [permissive-xexprs  (Parameterof False)]
  [xml->xexpr  (-> XML-Content XExpr)]
  [xexpr->xml  (-> XExpr XML-Content)]
  [xexpr->string  (-> XExpr String)]
  [string->xexpr  (-> String XExpr)]
  [xml-attribute-encode  (-> String String)]
  [eliminate-whitespace
   (->* () ((Listof Symbol) (-> Boolean Boolean)) (-> Element Element))]
  [validate-xexpr  (-> Any True)]
  [correct-xexpr?  (-> Any (-> Any) (-> Any Any) Any)]
  [current-unescaped-tags  (Parameterof (Listof Symbol))]
  [html-unescaped-tags  (Listof Symbol)]
  [empty-tag-shorthand  (Parameterof (U 'always 'never (Listof Symbol)))]
  [html-empty-tags  (Listof Symbol)]
  [collapse-whitespace  (Parameterof Boolean)]
  [read-comments  (Parameterof Boolean)]
  [xml-count-bytes  (Parameterof Boolean)]
  [xexpr-drop-empty-attributes  (Parameterof Boolean)])
