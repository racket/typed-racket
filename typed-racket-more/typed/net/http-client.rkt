#lang typed/racket/base

(define-type BString (U Bytes String))

(require (only-in typed/openssl
                  SSL-Client-Context))
;type for contract base-ssl?
(define-type Base-SSL (U Boolean Symbol SSL-Client-Context))
;type for contract base-ssl?-tnc
(define-type Base-SSL-Tnc (U Base-SSL
                             (List Base-SSL Input-Port Output-Port (-> Port Void))))
(require/typed/provide net/http-client
  [#:opaque HTTP-Connection http-conn?]
  [http-conn (-> HTTP-Connection)]
  [http-conn-live? (-> HTTP-Connection Boolean)]
  
  [http-conn-open!
   (->* [HTTP-Connection BString]
        [#:ssl? Base-SSL-Tnc #:port Positive-Integer]
        Void)]
  
  [http-conn-open 
   (->* [BString]
        [#:ssl? Base-SSL-Tnc #:port Positive-Integer]
        HTTP-Connection)]
  
  [http-conn-close! (-> HTTP-Connection Void)]
  [http-conn-abandon! (-> HTTP-Connection Void)]
  
  [http-conn-send!
   (->* [HTTP-Connection BString]
        [#:version BString #:method (U BString Symbol) #:close? Boolean #:headers (Listof BString)
                   #:content-decode (Listof Symbol) #:data (Option BString)]
        Void)]
  
  [http-conn-recv!
   (->* [HTTP-Connection]
        [#:content-decode (Listof Symbol) #:close? Boolean]
        (values Bytes (Listof Bytes) Input-Port))]
  
  [http-conn-sendrecv!
   (->* [HTTP-Connection BString]
        [#:version BString #:method (U BString Symbol) #:headers (Listof BString)
                   #:data (Option BString) #:content-decode (Listof Symbol) #:close? Boolean]
        (values Bytes (Listof Bytes) Input-Port))]
  
  [http-sendrecv
   (->* [BString BString]
        [#:ssl? Base-SSL-Tnc #:port Positive-Integer #:version BString
                #:method (U BString Symbol) #:headers (Listof BString) #:data (Option BString) 
                #:content-decode (Listof Symbol)]
        (values Bytes (Listof Bytes) Input-Port))]

  [http-conn-CONNECT-tunnel
   (->* [BString Positive-Fixnum BString Positive-Fixnum]
        [#:ssl? Base-SSL]
        (values Base-SSL Input-Port Output-Port (-> Port Void)))])
