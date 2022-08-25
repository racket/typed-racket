#lang s-exp typed-racket/base-env/extra-env-lang

;; This file provides types for the bindings from base-contracted.rkt
;; for non-Deep versions of TR that don't need to wrap the bindings

(type-environment
 [default-continuation-prompt-tag
   (-> (make-Prompt-Tagof Univ (-> Univ ManyUniv :T+ #t)) :T+ #t)])
