#lang typed/racket/base

(provide container)

(struct container
  ([value : (U #f container)])
  #:prefab)
