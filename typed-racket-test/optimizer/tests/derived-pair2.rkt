#;#;
#<<END
TR opt: derived-pair2.rkt 2:1 caaar -- pair
TR opt: derived-pair2.rkt 2:1 caaar -- pair
TR opt: derived-pair2.rkt 2:1 caaar -- pair
TR opt: derived-pair2.rkt 3:1 caadr -- pair
TR opt: derived-pair2.rkt 3:1 caadr -- pair
TR opt: derived-pair2.rkt 3:1 caadr -- pair
TR opt: derived-pair2.rkt 4:1 cadar -- pair
TR opt: derived-pair2.rkt 4:1 cadar -- pair
TR opt: derived-pair2.rkt 4:1 cadar -- pair
TR opt: derived-pair2.rkt 5:1 caddr -- pair
TR opt: derived-pair2.rkt 5:1 caddr -- pair
TR opt: derived-pair2.rkt 5:1 caddr -- pair
TR opt: derived-pair2.rkt 6:1 cdaar -- pair
TR opt: derived-pair2.rkt 6:1 cdaar -- pair
TR opt: derived-pair2.rkt 6:1 cdaar -- pair
TR opt: derived-pair2.rkt 7:1 cdadr -- pair
TR opt: derived-pair2.rkt 7:1 cdadr -- pair
TR opt: derived-pair2.rkt 7:1 cdadr -- pair
TR opt: derived-pair2.rkt 8:1 cddar -- pair
TR opt: derived-pair2.rkt 8:1 cddar -- pair
TR opt: derived-pair2.rkt 8:1 cddar -- pair
TR opt: derived-pair2.rkt 9:1 cdddr -- pair
TR opt: derived-pair2.rkt 9:1 cdddr -- pair
TR opt: derived-pair2.rkt 9:1 cdddr -- pair
END
#<<END
1
2
2
3
2
3
3
4

END
#lang typed/racket #:optimize
#reader typed-racket-test/optimizer/reset-port

(caaar (cons (cons (cons 1 2) 3) 4))
(caadr (cons 1 (cons (cons 2 3) 4)))
(cadar (cons (cons 1 (cons 2 3)) 4))
(caddr (cons 1 (cons 2 (cons 3 4))))
(cdaar (cons (cons (cons 1 2) 3) 4))
(cdadr (cons 1 (cons (cons 2 3) 4)))
(cddar (cons (cons 1 (cons 2 3)) 4))
(cdddr (cons 1 (cons 2 (cons 3 4))))
