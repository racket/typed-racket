#lang typed/racket/base

(require (only-in "../xml.rkt" XExpr))

(provide (all-defined-out))

(define-type Se-Path
  (Listof (U Symbol Keyword)))

(require/typed/provide xml/path
  [se-path*/list  (-> Se-Path XExpr (Listof Any))]
  [se-path*  (-> Se-Path XExpr Any)])
