#lang racket/base

(require typed-racket/utils/utils
         (prefix-in ru: rackunit)
         (for-syntax
          racket/base syntax/parse
          (utils tc-utils)
          (env init-envs)
          (rep prop-rep object-rep type-rep)
          (types abbrev)))

(define-for-syntax unit-env
  (make-env
   [ru:current-test-case-around
    (-poly (a) (-> (-> a) a))]))

(begin-for-syntax (initialize-type-env unit-env))
