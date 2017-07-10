#;#;
#<<END
TR opt: issue-577.rkt 16:0 (magnitude 0.0+0.0i) -- unboxed unary float complex
TR opt: issue-577.rkt 16:11 0.0+0.0i -- unboxed literal
TR opt: issue-577.rkt 17:0 (magnitude 1.6496437860480294e+295+1.7259537815112588e-137i) -- unboxed unary float complex
TR opt: issue-577.rkt 17:11 1.6496437860480294e+295+1.7259537815112588e-137i -- unboxed literal
END
#<<END
0.0
1.6496437860480294e+295

END

#lang typed/racket

(magnitude 0.0+0.0i)
(magnitude 1.6496437860480294e+295+1.7259537815112588e-137i)
