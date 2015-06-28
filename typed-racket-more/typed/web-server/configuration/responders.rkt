#lang typed/racket

(require "../http.rkt")

(require typed/net/url)

(require/typed/provide web-server/configuration/responders
                       [file-response (-> Natural Bytes Path-String Header * Response)]
                       [servlet-loading-responder (-> URL exn Response)]
                       [gen-servlet-not-found (-> Path-String (-> URL Response))]
                       [servlet-error-responder (-> URL exn Response)]
                       [gen-servlet-responder (-> Path-String (-> URL exn Response))]
                       [gen-servlets-refreshed (-> Path-String (-> Response))]
                       [gen-passwords-refreshed (-> Path-String (-> Response))]
                       [gen-authentication-responder (-> Path-String (-> URL Header Response))]
                       [gen-protocol-responder (-> Path-String (-> URL Response))]
                       [gen-file-not-found-responder (-> Path-String (-> Request Response))]
                       [gen-collect-garbage-responder (-> Path-String (-> Response))])
