#lang typed/racket/base

;; net/url-structs
(require typed/net/url-structs)
(provide (all-from-out typed/net/url-structs))

;; opaque types
(require/typed/provide
 net/url
 [#:opaque URL-Exception url-exception?]
 [#:opaque HTTP-Connection http-connection?])


;; convenience type aliases
;; PortT/String is unused, but was provided by earlier versions of this module, so it's included here
(define-type PortT ([URL] [(Listof String)] . ->* . Input-Port))
(define-type PortT/String ([URL] [String (Listof String)] . ->* . Input-Port))
(define-type PortT/Bytes ([URL] [Bytes (Listof String)] . ->* . Input-Port))
(provide PortT PortT/String PortT/Bytes)

(require/typed/provide
 net/url
 
 [string->url (String -> URL)]
 [combine-url/relative (URL String -> URL)]
 [netscape/string->url (String -> URL)]
 [url->string (URL -> String)]
 
 [path->url ((U Path-String Path-For-Some-System) -> URL)]
 [url->path ([URL] [(U 'unix 'windows)] . ->* . Path-For-Some-System)]
 [relative-path->relative-url-string ((U Path-String Path-For-Some-System) -> String)]
 
 [file-url-path-convention-type (Parameterof (U 'unix 'windows))]
 [current-url-encode-mode (Parameterof (U 'recommended 'unreserved))]
 
 [get-pure-port ([URL] [(Listof String) #:redirections Natural] . ->* . Input-Port)]
 [head-pure-port PortT]
 [delete-pure-port PortT]
 
 [get-impure-port PortT]
 [head-impure-port PortT]
 [delete-impure-port PortT]
 
 [post-pure-port PortT/Bytes]
 [put-pure-port PortT/Bytes]
 
 [post-impure-port PortT/Bytes]
 [put-impure-port PortT/Bytes]
 
 [display-pure-port (Input-Port -> Void)]
 [purify-port (Input-Port -> String)]
 
 [get-pure-port/headers ([URL]
                         [(Listof String)
                          #:redirections Natural
                          #:status? Boolean
                          #:connection (Option HTTP-Connection)]
                         . ->* . (Values Input-Port String))]
 
 [make-http-connection (-> HTTP-Connection)]
 [http-connection-close (HTTP-Connection -> Void)]
 
 [call/input-url
  (All [a] (case->
            [URL (URL -> Input-Port) (Input-Port -> a) -> a]
            [URL (URL (Listof String) -> Input-Port) (Input-Port -> a) (Listof String) -> a]))]
 
 [current-proxy-servers (Parameterof (Listof (List String String Integer)))]
 
 [http-sendrecv/url
  (URL
   [#:method (U Bytes String Symbol)]
   [#:headers (Listof (U Bytes String))]
   [#:data (Option (U Bytes String))]
   [#:content-decode (Listof Symbol)]
   -> (Values Bytes (Listof Bytes) Input-Port))])

