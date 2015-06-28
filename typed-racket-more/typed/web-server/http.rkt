#lang typed/racket

(require typed/net/url)

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
                                  [seconds : Natural]
                                  [mime : (Option Bytes)]
                                  [headers : (Listof Header)]
                                  [output : (-> Output-Port Void)])]
                       [response/full
                        (-> Natural
                            Bytes
                            Number
                            (Option Bytes)
                            (Listof Header)
                            (Listof Bytes)
                            Response)]
                       [response/output
                        (-> (-> Output-Port Void)
                            [#:code Natural]
                            [#:message Bytes]
                            [#:seconds Number]
                            [#:mime-type (Option Bytes)]
                            [#:headers (Listof Header)]
                            Response)]
                       [TEXT/HTML-MIME-TYPE Bytes])


(require/typed/provide net/cookie
                       [#:opaque Cookie cookie?]
                       [#:opaque Cookie-Name cookie-name?]
                       [#:opaque Cookie-Value cookie-value?]
                       [#:opaque Valid-Domain valid-domain?])

(require/typed/provide web-server/http/cookie
                       [make-cookie
                        (-> Cookie-Name Cookie-Value
                            [#:comment (Option String)]
                            [#:domain (Option Valid-Domain)]
                            [#:max-age (Option Natural)]
                            [#:path (Option String)]
                            [#:expires (Option String)]	 	 	 	 
                            [#:secure? (Option Boolean)]
                            Cookie)]
                       [cookie->header
                        (-> Cookie
                            Header)])

(require/typed/provide web-server/http/id-cookie
                       [make-id-cookie
                        (-> Cookie-Name
                            Bytes
                            Cookie-Value
                            [#:path (Option String)]
                            Cookie)]
                       [request-id-cookie
                        (-> Cookie-Name
                            Bytes
                            Request
                            #:timeout Number
                            (Option Cookie-Value))]
                       [logout-id-cookie
                        (-> Cookie-Name
                            [#:path (Option String)]
                            Cookie)]
                       [make-secret-salt/file
                        (-> Path-String
                            Bytes)])

(require/typed/provide web-server/http/cookie-parse
                       [#:struct client-cookie
                                 ([name : String]
                                  [value : String]
                                  [domain : (Option Valid-Domain)]
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
                            [#:seconds Number]
                            [#:mime-type (Option Bytes)]
                            [#:headers (Listof Header)]
                            [#:cookies (Listof Cookie)]
                            [#:preamble Bytes]
                            Response)])
