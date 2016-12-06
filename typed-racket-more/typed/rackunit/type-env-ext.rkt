#lang racket/base

(require typed-racket/utils/utils
         (prefix-in ru: (combine-in rackunit rackunit/private/test-case rackunit/private/check))
         (for-syntax
          racket/base syntax/parse
          (utils tc-utils)
          (env init-envs)
          (except-in (rep prop-rep object-rep type-rep) make-arr)
          (rename-in (types abbrev) [make-arr* make-arr])))

(define-for-syntax unit-env
  (make-env
   [ru:check-around
    (-poly (a) (-> (-> a) a))]
   [ru:current-test-case-around
    (-poly (a) (-> (-> a) a))]))

(begin-for-syntax (initialize-type-env unit-env))
