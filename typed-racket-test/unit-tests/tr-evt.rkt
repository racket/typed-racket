#lang racket/base

(require rackunit
         typed-racket/utils/evt-contract
         racket/contract)

(check-true  (contract-stronger?   (tr:evt/c (</c 10)) (tr:evt/c (</c 11))))
(check-false (contract-stronger?   (tr:evt/c (</c 11)) (tr:evt/c (</c 10))))
(check-true  (contract-stronger?   (tr:evt/c (</c 10)) (tr:evt/c (</c 10))))
(check-false (contract-stronger?   (tr:evt/c (</c 10)) (</c 10)))
(check-false (contract-equivalent? (tr:evt/c (</c 10)) (tr:evt/c (</c 11))))
(check-false (contract-equivalent? (tr:evt/c (</c 11)) (tr:evt/c (</c 10))))
(check-true  (contract-equivalent? (tr:evt/c (</c 10)) (tr:evt/c (</c 10))))
(check-false (contract-equivalent? (tr:evt/c (</c 10)) (</c 10)))
