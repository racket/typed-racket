#lang typed/racket/base

(require typed/private/utils typed/racket/unsafe)

(unsafe-require/typed/provide net/head
  [extract-field (case-> (Bytes Bytes -> (Option Bytes))
                         (String String -> (Option String)))]
  [remove-field (case-> (Bytes Bytes -> Bytes)
                        (String String -> String))]
  [insert-field (case-> (Bytes Bytes Bytes -> Bytes)
                        (String String String -> String))]
  [replace-field (case-> (Bytes Bytes Bytes -> Bytes)
                         (String String String -> String))]
  [extract-all-fields (case-> (Bytes -> (Listof (cons Bytes Bytes)))
                              (String -> (Listof (cons String String))))]
  [append-headers (case-> (Bytes Bytes -> Bytes)
                          (String String -> String))])
(require/typed/provide net/head
  [empty-header String]
  [validate-header ((U String Bytes) -> Void)]
  [standard-message-header (String (Listof String) (Listof String) (Listof String) String -> String)]
  [data-lines->data ((Listof String) -> String)]
  [extract-addresses (String Symbol -> (U (Listof String) (Listof (Listof String))))]
  [assemble-address-field ((Listof String) -> String)])

(provide
 empty-header
 validate-header
 extract-field
 remove-field
 insert-field
 replace-field
 extract-all-fields
 append-headers
 standard-message-header
 data-lines->data
 extract-addresses
 assemble-address-field)
