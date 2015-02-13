#lang typed/racket/base

(require typed/private/utils)

(require/typed/provide net/uri-codec
  [uri-encode ( String -> String )]
  [uri-decode ( String -> String )]

  [uri-path-segment-encode (-> String String)]
  [uri-path-segment-decode (-> String String)]

  [uri-userinfo-encode (-> String String)]
  [uri-userinfo-decode (-> String String)]

  [uri-unreserved-encode (-> String String)]
  [uri-unreserved-decode (-> String String)]

  [uri-path-segment-unreserved-encode (-> String String)]
  [uri-path-segment-unreserved-decode (-> String String)]

  [form-urlencoded-encode ( String -> String )]
  [form-urlencoded-decode ( String -> String )]

  [alist->form-urlencoded ( (Listof (cons Symbol String)) -> String )]
  [form-urlencoded->alist ( String -> (Listof (cons Symbol String)) )]
  [current-alist-separator-mode (Parameter (U 'amp 'semi 'amp-or-semi 'semi-or-amp))])
