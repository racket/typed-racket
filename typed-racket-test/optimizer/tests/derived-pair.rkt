#;#;
#<<END
TR opt: derived-pair.rkt 2:1 caar -- pair
TR opt: derived-pair.rkt 2:1 caar -- pair
TR opt: derived-pair.rkt 3:1 cadr -- pair
TR opt: derived-pair.rkt 3:1 cadr -- pair
TR opt: derived-pair.rkt 4:1 cdar -- pair
TR opt: derived-pair.rkt 4:1 cdar -- pair
TR opt: derived-pair.rkt 5:1 cddr -- pair
TR opt: derived-pair.rkt 5:1 cddr -- pair
END
#<<END
1
2
2
3

END
#lang typed/racket #:optimize
#reader typed-racket-test/optimizer/reset-port

(caar (cons (cons 1 2) 3))
(cadr (cons 1 (cons 2 3)))
(cdar (cons (cons 1 2) 3))
(cddr (cons 1 (cons 2 3)))
