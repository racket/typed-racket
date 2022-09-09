#lang racket

#;(void (putenv "PLT_TR_CONTRACTS" "true"))

(define ns (make-base-namespace))
(current-namespace ns)
(use-compiled-file-paths null)

#;
((dynamic-require 'typed-racket-test/main 'go/text)
 (dynamic-require 'typed-racket-test/main 'unit-tests))
