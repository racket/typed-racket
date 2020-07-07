#lang typed/racket/base

(require typed/rackunit)

(check-equal? (magnitude +inf.0+1.0i) +inf.0)
(check-equal? (magnitude +inf.0-inf.0i) +inf.0)
(check-equal? (magnitude +nan.0-inf.0i) +inf.0)
(check-equal? (magnitude -inf.0-inf.0i) +inf.0)
(check-equal? (magnitude +inf.0+nan.0i) +inf.0)
