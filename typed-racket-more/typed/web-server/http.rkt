#lang typed/racket

(require typed/net/url
         typed/racket/date)

(provide (all-defined-out))

(define-type Header header)
(define-type Binding binding)
(define-type Request request)
(define-type Response response)
(define-type Client-Cookie client-cookie)

(define-type Digest-Credentials (Listof (Pairof Symbol String)))
(define-type Username*Realm->Password (-> String String String))
(define-type Username*Realm->Digest-HA1 (-> String String Bytes))

(require/typed/provide web-server/http/request-structs
                       [#:struct header
                                 ([field : Bytes]
                                  [value : Bytes])
                                 #:extra-constructor-name make-header]
                       [#:struct binding
                                 ([id : Bytes])
                                 #:extra-constructor-name make-binding]
                       [#:struct (binding:form binding)
                                 ([value : Bytes])
                                 #:extra-constructor-name make-binding:form]
                       [#:struct (binding:file binding)
                                 ([filename : Bytes]
                                  [headers : (Listof Header)]
                                  [content : Bytes])
                                 #:extra-constructor-name make-binding:file]
                       [#:struct request
                                 ([method : Bytes]
                                  [uri : URL]
                                  [headers/raw : (Listof Header)]
                                  [bindings/raw-promise : (Promise (Listof Binding))]
                                  [post-data/raw : (Option Bytes)]
                                  [host-ip : String]
                                  [host-port : Natural]
                                  [client-ip : String])
                                 #:extra-constructor-name make-request]
                       [headers-assq*
                        (-> Bytes
                            (Listof Header)
                            (Option Header))]
                       [bindings-assq
                        (-> Bytes
                            (Listof Binding)
                            (Option Binding))]
                       [bindings-assq-all
                        (-> Bytes
                            (Listof Binding)
                            (Listof Binding))])

(require/typed/provide web-server/http/response-structs
                       [#:struct response
                                 ([code : Natural]
                                  [message : Bytes]
                                  [seconds : Real]
                                  [mime : (Option Bytes)]
                                  [headers : (Listof Header)]
                                  [output : (-> Output-Port Any)])]
                       [response/full
                        (-> Natural
                            Bytes
                            Real
                            (Option Bytes)
                            (Listof Header)
                            (Listof Bytes)
                            Response)]
                       [response/output
                        (-> (-> Output-Port Any)
                            [#:code Natural]
                            [#:message Bytes]
                            [#:seconds Real]
                            [#:mime-type (Option Bytes)]
                            [#:headers (Listof Header)]
                            Response)]
                       [TEXT/HTML-MIME-TYPE Bytes])

;; For backwards compatability:
;; these bindings are not and never were
;; exported by web-server/http,
;; but they were exported by previous versions of
;; typed/web-server/http based on net/cookie.

(require (only-in "../net/cookies/server.rkt"
                  Cookie
                  cookie?)
         (only-in "../net/cookies/common.rkt"
                  cookie-name?
                  cookie-value?
                  [domain-value? valid-domain?]))

(define-type Cookie-Name String)
(define-type Cookie-Value String)
(define-type Valid-Domain String)

(provide (all-from-out "../net/cookies/server.rkt")
         (all-from-out "../net/cookies/common.rkt")
         Cookie-Name
         Cookie-Value
         Valid-Domain)

(require/typed/provide
 web-server/http/cookie
 [make-cookie
  (-> String String
      [#:comment Any]
      [#:domain (Option String)]
      [#:max-age (Option Natural)]
      [#:path (Option String)]
      [#:expires (Option (U date String))]
      [#:extension (Option String)]
      ;; yes, these are really different
      ;; than for typed/net/cookies/server
      [#:secure? Any]
      [#:http-only? Any]
      Cookie)]
 [cookie->header
  (-> Cookie
      Header)])

(require/typed/provide web-server/http/id-cookie
                       [make-id-cookie
                        (-> String
                            Bytes
                            String
                            [#:path (Option String)]
                            [#:expires (Option date)]
                            [#:max-age (Option Exact-Positive-Integer)]
                            [#:domain (Option String)]
                            [#:extension (Option String)]
                            ;; yes, these are really different
                            ;; than for typed/net/cookies/server
                            [#:secure? Any]	 	 	 	 
                            [#:http-only? Any]
                            Cookie)]
                       [request-id-cookie
                        (-> String
                            Bytes
                            Request
                            [#:timeout Number]
                            [#:shelf-life Real]
                            (Option String))]
                       [valid-id-cookie?
                        (-> Any ;; yes, this is really Any
                            #:name String
                            #:key Bytes
                            [#:timeout Number]
                            [#:shelf-life Real]
                            (Option String))]
                       [logout-id-cookie
                        (-> String
                            [#:path (Option String)]
                            [#:domain (Option String)]
                            Cookie)]
                       [make-secret-salt/file
                        (-> Path-String
                            Bytes)])

(require/typed/provide web-server/http/cookie-parse
                       [#:struct client-cookie
                                 ([name : String]
                                  [value : String]
                                  [domain : (Option String)]
                                  [path : (Option String)])]
                       [request-cookies
                        (-> Request
                            (Listof Client-Cookie))])

(require/typed/provide web-server/http/redirect
                       [#:opaque Redirection-Status redirection-status?]
                       [redirect-to
                        (->* (String)
                             (Redirection-Status #:headers (Listof Header))
                             Response)]
                       [permanently Redirection-Status]
                       [temporarily Redirection-Status]
                       [temporarily/same-method Redirection-Status]
                       [see-other Redirection-Status])

(require/typed/provide web-server/http/basic-auth
                       [make-basic-auth-header
                        (-> String
                            Header)]
                       [request->basic-credentials
                        (-> Request
                            (Option (Pairof Bytes Bytes)))])

(require/typed/provide web-server/http/digest-auth
                       [make-digest-auth-header
                        (-> String
                            String
                            String
                            Header)]
                       [request->digest-credentials
                        (-> Request
                            (Option Digest-Credentials))]
                       [make-check-digest-credentials
                        (-> Username*Realm->Digest-HA1
                            (-> String Digest-Credentials Boolean))]
                       [password->digest-HA1
                        (-> Username*Realm->Password
                            Username*Realm->Digest-HA1)])

(require/typed/provide web-server/http/xexpr
                       [response/xexpr
                        (-> Any  ;;; it should be `xexpr?`ed value, but `Any` also works well.
                            [#:code Natural]
                            [#:message Bytes]
                            [#:seconds Real]
                            [#:mime-type (Option Bytes)]
                            [#:headers (Listof Header)]
                            [#:cookies (Listof Cookie)]
                            [#:preamble Bytes]
                            Response)])
