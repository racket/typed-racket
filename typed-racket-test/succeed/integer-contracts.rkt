#lang typed/racket/base

(cast 2 Integer)
(cast 2 Positive-Integer)
(cast -2 Negative-Integer)
(cast 2 Nonnegative-Integer)
(cast -2 Nonpositive-Integer)
