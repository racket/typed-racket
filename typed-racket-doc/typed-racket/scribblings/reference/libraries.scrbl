#lang scribble/manual

@begin[(require "../utils.rkt")
       (require (for-label (only-meta-in 0 [except-in typed/racket for])
                           (only-in racket/base for)
                           racket/list srfi/14 net/url
                           version/check
                           ;; Specific libraries wrapped for TR:
                           file/gif
                           net/http-client
                           net/url-structs
                           net/url
                           openssl
                           json))]


@title{Libraries Provided With Typed Racket}

The @racketmodname[typed/racket] language corresponds to the
@racketmodname[racket] language---that is, any identifier provided
by @racketmodname[racket], such as @racket[modulo], is available by default in
@racketmodname[typed/racket].

@racketmod[typed/racket
(modulo 12 2)
]

The @racketmodname[typed/racket/base] language corresponds to the
@racketmodname[racket/base] language.

Some libraries have counterparts in the @racketidfont{typed}
collection, which provide the same exports as the untyped versions.
Such libraries include @racketmodname[srfi/14],
@racketmodname[net/url], and many others.

@racketmod[typed/racket
(require typed/srfi/14)
(char-set= (string->char-set "hello")
           (string->char-set "olleh"))
]

Other libraries can be used with Typed Racket via
@racket[require/typed].

@racketmod[typed/racket
(require/typed version/check
               [check-version (-> (U Symbol (Listof Any)))])
(check-version)
]

The following libraries are included with Typed Racket in the
@racketfont{typed} collection:

@(define-syntax-rule @defmodule/incl[name]
   @defmodule[name #:no-declare])

@(define-syntax-rule (deftype name . parts)
   (defidform #:kind "type" name . parts))

@;; framework and mred left out until support for classes
@;; is more complete
@defmodule/incl[typed/file/gif]
@deftype[GIF-Stream]{
  Describe a GIF stream, as produced by @racket[gif-start]
  and accepted by the other functions from @racketmodname[file/gif].
}
@deftype[GIF-Colormap]{
  Type alias for a list of three-element (R,G,B) vectors representing an image.
}

@defmodule/incl[typed/file/md5]
@defmodule/incl[typed/file/tar]
@defmodule/incl[typed/framework]
@defmodule/incl[typed/json]

@deftype[JSExpr]{
  Describes a @tech["jsexpr" #:doc '(lib "json/json.scrbl")].
}

@defmodule/incl[typed/mred/mred]
@defmodule/incl[typed/net/base64]
@defmodule/incl[typed/net/cgi]
@defmodule/incl[typed/net/cookie]

@deftype[Cookie]{
  Describes an HTTP cookie as implemented by @racketmodname[net/cookie].
}

@defmodule/incl[typed/net/dns]
@defmodule/incl[typed/net/ftp]

@deftype[FTP-Connection]{
  Describes an open FTP connection.
}

@defmodule/incl[typed/net/gifwrite]
@defmodule/incl[typed/net/git-checkout]
@defmodule/incl[typed/net/head]
@defmodule/incl[typed/net/http-client]

@deftype[HTTP-Connection]{
  Describes an HTTP connection, corresponding to @racket[http-conn?].
}

@defmodule/incl[typed/net/imap]

@deftype[IMAP-Connection]{
  Describes an IMAP connection.
}

@defmodule/incl[typed/net/mime]
@defmodule/incl[typed/net/nntp]
@defmodule/incl[typed/net/pop3]
@defmodule/incl[typed/net/qp]
@defmodule/incl[typed/net/sendmail]
@defmodule/incl[typed/net/sendurl]
@defmodule/incl[typed/net/smtp]
@defmodule/incl[typed/net/uri-codec]
@defmodule/incl[typed/net/url-connect]
@defmodule/incl[typed/net/url-structs]

@deftype[Path/Param]{
  Describes the @racket[path/param] struct from @racketmodname[net/url-structs].
}
@deftype[URL]{
  Describes an @racket[url] struct from @racketmodname[net/url-structs].
}

@defmodule/incl[typed/net/url]
In addition to defining the following types, this module also provides the
@racket[HTTP-Connection] type defined by @racketmodname[typed/net/http-client],
and the @racket[URL] and @racket[Path/Param] types from
@racketmodname[typed/net/url-structs].

@deftype[URL-Exception]{
  Describes exceptions raised by URL-related functions; corresponds
  to @racket[url-exception?].
}
@deftype[PortT]{
  Describes the functions @racket[head-pure-port], @racket[delete-pure-port],
  @racket[get-impure-port], @racket[head-impure-port], and
  @racket[delete-impure-port].
}
@deftype[PortT/Bytes]{
  Like @racket[PortT], but describes the functions that make POST and PUT
  requests, which require an additional byte-string argument for POST or PUT
  data.
}

@defmodule/incl[typed/openssl]

@deftype[SSL-Protocol]{
  Describes an SSL protocol, defined as
  @racket[(U 'auto 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)].
}
@deftogether[(@deftype[SSL-Server-Context]
              @deftype[SSL-Client-Context])]{
  Describes an OpenSSL server or client context.
}
@deftype[SSL-Context]{Supertype of OpenSSL server and client contexts.}
@deftype[SSL-Listener]{
  Describes an SSL listener, as produced by @racket[ssl-listen].
}
@deftype[SSL-Verify-Source]{
  Describes a verification source usable by @racket[ssl-load-verify-source!]
  and the @racket[ssl-default-verify-sources] parameter.
}

@defmodule/incl[typed/openssl/md5]
@defmodule/incl[typed/openssl/sha1]
@defmodule/incl[typed/pict]
@defmodule[typed/racket/async-channel #:no-declare @history[#:added "1.1"]]
@defmodule/incl[typed/racket/date]
@defmodule/incl[typed/racket/draw]
@defmodule/incl[typed/racket/gui]
@defmodule/incl[typed/racket/sandbox]
@defmodule/incl[typed/racket/snip]
@defmodule/incl[typed/racket/system]
@defmodule/incl[typed/rackunit/docs-complete]
@defmodule/incl[typed/rackunit/gui]
@defmodule/incl[typed/rackunit/text-ui]
@defmodule/incl[typed/rackunit]
@defmodule/incl[typed/srfi/14]

@deftype[Char-Set]{
  Describes a character set usable by the @racketmodname[srfi/14] functions.
}
@deftype[Cursor]{
  Describes a cursor for iterating over character sets.
}

@defmodule/incl[typed/srfi/19]

@deftogether[(@defidform[#:kind "type" Time]
              @defidform[#:kind "type" Date])]{
  Describes an SRFI 19 time or date structure.
}

@defmodule/incl[typed/syntax/stx]
@defmodule/incl[typed/web-server/configuration/responders]
@defmodule/incl[typed/web-server/http]

In some cases, these typed adapters may not contain all of exports of the
original module, or their types may be more limited.

Other libraries included in the main distribution that are either
written in Typed Racket or have adapter modules that are typed:

@(define-syntax-rule @defmodule/also[name]
   @defmodule[name #:no-declare #:link-target? #f #:indirect])

@defmodule/also[math]
@defmodule/also[plot/typed]

@section{Porting Untyped Modules to Typed Racket}

To adapt a Racket library not included with Typed Racket, the
following steps are required:

@itemlist[
  @item{Determine the data manipulated by the library, and how it will
  be represented in Typed Racket.}
  @item{Specify that data in Typed Racket, using @racket[require/typed]
  and @racket[#:opaque] and/or @racket[#:struct].}
  @item{Use the data types to import the various functions and constants
  of the library.}
  @item{Provide all the relevant identifiers from the new adapter module.}
]

For example, the following module adapts the untyped
@racketmodname[racket/bool] library:

@racketmod[typed/racket
  (require/typed racket/bool
                 [true Boolean]
                 [false Boolean]
                 [symbol=? (Symbol Symbol -> Boolean)]
                 [boolean=? (Boolean Boolean -> Boolean)]
                 [false? (Any -> Boolean)])
  (provide true false symbol=? boolean=? false?)
]

More substantial examples are available in the @racketfont{typed}
collection.

