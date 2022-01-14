#lang typed/racket/base

(provide (all-defined-out))

(define-type Plist-Value
  (U Plist-Dict
     String
     (List 'data String)
     (List 'date String)
     (List 'false)
     (List 'integer Integer)
     (List 'real Real)
     (List 'true)
     (Pair 'array (Listof Plist-Value))))

(define-type Plist-Dict
  (Pair 'dict (Listof Assoc-Pair)))

(define-type Assoc-Pair
  (List 'assoc-pair String Plist-Value))

(require/typed/provide xml/plist
  [plist-value? (-> Any Boolean)]
  [plist-dict? (-> Any Boolean)]
  [read-plist (-> Input-Port Plist-Value)]
  [write-plist (-> Plist-Dict Output-Port Void)])
