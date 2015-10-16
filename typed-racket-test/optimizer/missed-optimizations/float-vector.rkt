#;#;
#<<END
TR info: float-vector.rkt 2:0 (vector 3.2 3.4) -- vector of floats
END
#<<END
'#(3.2 3.4)

END

#lang typed/racket
#reader typed-racket-test/optimizer/reset-port

(vector 3.2 3.4)
