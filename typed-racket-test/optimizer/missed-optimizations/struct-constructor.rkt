#;#;
#<<END
TR info: struct-constructor.rkt 6:7 foo -- struct constructor
END
""
#lang typed/racket
#reader typed-racket-test/optimizer/reset-port

(provide (struct-out foo))

(struct: foo ())

(void (foo))
