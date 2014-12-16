#;#;
#<<END
TR missed opt: sequence.rkt 4:22 s -- non-specialized for clause
END
""
#lang typed/racket/base
#reader typed-racket-test/optimizer/reset-port

(: seq-generic : (Sequenceof Integer) -> Void)
(define (seq-generic s)
  (for: ([x : Integer s])
    (void)))
