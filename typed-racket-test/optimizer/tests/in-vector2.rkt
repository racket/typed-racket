#;#;
#<<END
TR info: in-vector2.rkt 2:7 display -- hidden parameter
TR opt: in-vector2.rkt 1:25 (vector 1 2 3) -- in-vector
END
"123"
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port
(for: ((i : Integer (ann (vector 1 2 3) (Vectorof Integer))))
      (display i))
