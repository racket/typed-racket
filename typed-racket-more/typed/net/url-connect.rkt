#lang typed/racket/base

(require typed/openssl
         typed/openssl/types)

(require/typed/provide
 net/url-connect
 [current-https-protocol (U SSL-Client-Context SSL-Protocol)])
