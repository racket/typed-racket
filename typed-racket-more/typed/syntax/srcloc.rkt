#lang typed/racket/base

(provide Source-Location
         Source-Location-List
         Source-Location-Vector
         source-location?
         source-location-list?
         source-location-vector?
         check-source-location!
         build-source-location
         build-source-location-list
         build-source-location-vector
         build-source-location-syntax
         source-location-known?
         source-location-source
         source-location-line
         source-location-column
         source-location-position
         source-location-span
         source-location-end
         update-source-location
         source-location->string
         source-location->prefix
         )

(require typed/racket/unsafe)

(define-type Source-Location
  (U srcloc
     (Syntaxof Any)
     Source-Location-List
     Source-Location-Vector
     False))

(define-type Source-Location-List
  (List Any
        (U Positive-Integer False)
        (U Natural False)
        (U Positive-Integer False)
        (U Natural False)))

(define-type Source-Location-Vector
  (Vector Any
          (U Positive-Integer False)
          (U Natural False)
          (U Positive-Integer False)
          (U Natural False)))

(unsafe-require/typed syntax/srcloc
                      [source-location? (-> Any Boolean)]
                      [source-location-list? (-> Any Boolean)]
                      [source-location-vector? (-> Any Boolean)]
                      [check-source-location! (-> Symbol Any Void)]
                      [build-source-location (-> Source-Location * srcloc)]
                      [build-source-location-list (-> Source-Location * Source-Location-List)]
                      [build-source-location-vector (-> Source-Location * Source-Location-Vector)]
                      [build-source-location-syntax (-> Source-Location * (Syntaxof Any))]
                      [source-location-known? (-> Source-Location Boolean)]
                      [source-location-source (-> Source-Location Any)]
                      [source-location-line (-> Source-Location (U Positive-Integer False))]
                      [source-location-column (-> Source-Location (U Natural False))]
                      [source-location-position (-> Source-Location (U Positive-Integer False))]
                      [source-location-span (-> Source-Location (U Natural False))]
                      [source-location-end (-> Source-Location (U Natural False))]
                      [update-source-location (-> Source-Location
                                                  [#:source Any]
                                                  [#:line (U Positive-Integer False)]
                                                  [#:column (U Natural False)]
                                                  [#:position (U Positive-Integer False)]
                                                  [#:span (U Natural False)]
                                                  Source-Location)]
                      [source-location->string (-> Source-Location String)]
                      [source-location->prefix (-> Source-Location String)]
                      )
