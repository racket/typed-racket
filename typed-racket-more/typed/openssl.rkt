#lang typed/racket/base

(require "openssl/types.rkt")
(provide (all-from-out "openssl/types.rkt"))

(require/typed/provide openssl
  [ssl-available? Boolean]
  [ssl-load-fail-reason (Option String)]
  
  ;; 1: TCP-like Client Procedures
  [ssl-connect
   (->* (String Exact-Positive-Integer)
        ((U SSL-Client-Context SSL-Protocol)
         #:alpn (Listof Bytes))
        (Values Input-Port Output-Port))]
  [ssl-connect/enable-break
   (->* (String Exact-Positive-Integer)
        ((U SSL-Client-Context SSL-Protocol)
         #:alpn (Listof Bytes))
        (Values Input-Port Output-Port))]
  
  [ssl-secure-client-context (-> SSL-Client-Context)]
  [ssl-make-client-context
   (->* ()
        (SSL-Protocol
         #:private-key SSL-Private-Key
         #:certificate-chain (Option Path-String))
        SSL-Client-Context)]
  [supported-client-protocols (-> (Listof SSL-Protocol))]
  [ssl-max-client-protocol (-> (Option SSL-Protocol))]
  )

;;;; 2: TCP-like Server Procedures
(require/typed/provide openssl
  [ssl-listen (->* (Exact-Nonnegative-Integer) ;; port, <= 65535
                   (Exact-Nonnegative-Integer Boolean (Option String) (U SSL-Server-Context SSL-Protocol))
                   SSL-Listener)]
  [ssl-close (-> SSL-Listener Void)]
  ;; ssl-listener? provided above
  
  [ssl-accept
   (-> SSL-Listener (Values Input-Port Output-Port))]
  [ssl-accept/enable-break
   (-> SSL-Listener (Values Input-Port Output-Port))]
  [ssl-abandon-port (-> Port Void)] ;; XXX SSL-Port

  [ssl-make-server-context
   (->* ()
        (SSL-Protocol
         #:private-key SSL-Private-Key
         #:certificate-chain (Option Path-String))
        SSL-Server-Context)]
  [ssl-addresses
   (case->
    (-> (U Port SSL-Listener) (Values String String))
    (-> (U Port SSL-Listener) #t (Values String Index String Index)))]

  [supported-server-protocols (-> (Listof SSL-Protocol))]
  [ssl-max-server-protocol (-> (Option SSL-Protocol))]
  )

;;;; 3: SSL Wrapper Interface

(require/typed/provide openssl
  [ports->ssl-ports
   (-> Input-Port Output-Port
       [#:mode (U 'connect 'accept)]
       [#:context (U SSL-Client-Context SSL-Server-Context)]
       [#:encrypt SSL-Protocol]
       [#:close-original? Boolean]
       [#:shutdown-on-close? Boolean]
       [#:error/ssl (Any -> Void)] ;; FIXME find type for error proc
       [#:hostname (Option String)]
       [#:alpn (Listof Bytes)]
       ; ->
       (Values Input-Port Output-Port))]
  )

;;;; 4: Context Procedures

(require/typed/provide openssl
  [ssl-load-verify-source!
   (-> SSL-Context SSL-Verify-Source [#:try? Any] Void)]
  [ssl-default-verify-sources (Parameterof (Listof SSL-Verify-Source))]
  [ssl-load-default-verify-sources! (-> SSL-Context Void)]
  [ssl-load-verify-root-certificates!
   (-> (U SSL-Context SSL-Listener) Path-String Void)]
  [ssl-set-ciphers! (-> SSL-Context String Void)]
  [ssl-seal-context! (-> SSL-Context Void)]
  [ssl-load-certificate-chain!
   (-> (U SSL-Context SSL-Listener) Path-String Void)]
  [ssl-load-private-key!
   (->* ((U SSL-Context SSL-Listener)
         (U Path-String (List 'data Bytes)))
        (Boolean Boolean)
        Void)]
  [ssl-load-suggested-certificate-authorities!
   (-> (U SSL-Context SSL-Listener) Path-String Void)]
  [ssl-set-server-name-identification-callback!
   (-> SSL-Server-Context (-> String (Option SSL-Server-Context)) Void)]
  [ssl-set-server-alpn!
   (->* (SSL-Server-Context (Listof Bytes)) (Boolean) Void)]
  [ssl-set-keylogger! (-> SSL-Context (Option Logger) Void)]
  )

;;;; 5: Peer Verification

(require/typed/provide openssl
  [ssl-set-verify!
   (-> (U SSL-Context SSL-Listener Port) Any ;; XXX SSL-Port
       Void)]
  [ssl-try-verify!
   (-> (U SSL-Context SSL-Listener Port) Any ;; XXX SSL-Port
       Void)]
  [ssl-peer-verified? (-> Port Boolean)] ;; XXX SSL-Port
  [ssl-set-verify-hostname! (-> SSL-Context Any Void)]
  [ssl-peer-certificate-hostnames
   (-> Port (Listof String))]  ;; XXX SSL-Port
  [ssl-peer-check-hostname
   (-> Port String Boolean)]  ;; XXX SSL-Port
  [ssl-peer-subject-name (-> Port (Option Bytes))] ;; XXX SSL-Port
  [ssl-peer-issuer-name  (-> Port (Option Bytes))] ;; XXX SSL-Port
  [ssl-default-channel-binding (-> Port (List Symbol Bytes))]
  [ssl-channel-binding
   (-> Port (U 'tls-exporter 'tls-unique 'tls-server-end-point) Bytes)]
  [ssl-get-alpn-selected (-> Port (Option Bytes))]
  [ssl-protocol-version (-> Port (Option SSL-Protocol))]
  )

