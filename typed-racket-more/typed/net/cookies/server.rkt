#lang typed/racket/base

(require typed/racket/date)

(require/typed/provide
 net/cookies/server
 #| ; a cookie is this, but the cookie constructor isn't provided
 [#:struct cookie
  ([name : String]
   [value : String]
   [expires : (U #f Date)]
   [max-age : (U #f Exact-Positive-Integer)]
   [domain : (U #f String)]
   [path : (U #f String)]
   [secure? : Boolean] ;; yes, really not Any
   [http-only? : Boolean] ;; yes, really not Any
   [extension : (U #f String)])]
  |#
 [#:opaque Cookie cookie?]
 [make-cookie
  (->* [String String]
       [#:expires (U #f date)	 	 	 	 
        #:max-age (U #f Exact-Positive-Integer)	 	 	 	 
        #:domain (U #f String)	 	 	 	 
        #:path (U #f String)	 	 	 	 
        #:secure? Boolean ;; yes, really not Any	 	 	 	 
        #:http-only? Boolean ;; yes, really not Any	 	 	 	 
        #:extension (U #f String)]
       Cookie)]
 )
