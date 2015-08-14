#;#;
#<<END
TR opt: derived-pair3.rkt 10:1 cdaaar -- pair
TR opt: derived-pair3.rkt 10:1 cdaaar -- pair
TR opt: derived-pair3.rkt 10:1 cdaaar -- pair
TR opt: derived-pair3.rkt 10:1 cdaaar -- pair
TR opt: derived-pair3.rkt 11:1 cdaadr -- pair
TR opt: derived-pair3.rkt 11:1 cdaadr -- pair
TR opt: derived-pair3.rkt 11:1 cdaadr -- pair
TR opt: derived-pair3.rkt 11:1 cdaadr -- pair
TR opt: derived-pair3.rkt 12:1 cdadar -- pair
TR opt: derived-pair3.rkt 12:1 cdadar -- pair
TR opt: derived-pair3.rkt 12:1 cdadar -- pair
TR opt: derived-pair3.rkt 12:1 cdadar -- pair
TR opt: derived-pair3.rkt 13:1 cdaddr -- pair
TR opt: derived-pair3.rkt 13:1 cdaddr -- pair
TR opt: derived-pair3.rkt 13:1 cdaddr -- pair
TR opt: derived-pair3.rkt 13:1 cdaddr -- pair
TR opt: derived-pair3.rkt 14:1 cddaar -- pair
TR opt: derived-pair3.rkt 14:1 cddaar -- pair
TR opt: derived-pair3.rkt 14:1 cddaar -- pair
TR opt: derived-pair3.rkt 14:1 cddaar -- pair
TR opt: derived-pair3.rkt 15:1 cddadr -- pair
TR opt: derived-pair3.rkt 15:1 cddadr -- pair
TR opt: derived-pair3.rkt 15:1 cddadr -- pair
TR opt: derived-pair3.rkt 15:1 cddadr -- pair
TR opt: derived-pair3.rkt 16:1 cdddar -- pair
TR opt: derived-pair3.rkt 16:1 cdddar -- pair
TR opt: derived-pair3.rkt 16:1 cdddar -- pair
TR opt: derived-pair3.rkt 16:1 cdddar -- pair
TR opt: derived-pair3.rkt 17:1 cddddr -- pair
TR opt: derived-pair3.rkt 17:1 cddddr -- pair
TR opt: derived-pair3.rkt 17:1 cddddr -- pair
TR opt: derived-pair3.rkt 17:1 cddddr -- pair
TR opt: derived-pair3.rkt 2:1 caaaar -- pair
TR opt: derived-pair3.rkt 2:1 caaaar -- pair
TR opt: derived-pair3.rkt 2:1 caaaar -- pair
TR opt: derived-pair3.rkt 2:1 caaaar -- pair
TR opt: derived-pair3.rkt 3:1 caaadr -- pair
TR opt: derived-pair3.rkt 3:1 caaadr -- pair
TR opt: derived-pair3.rkt 3:1 caaadr -- pair
TR opt: derived-pair3.rkt 3:1 caaadr -- pair
TR opt: derived-pair3.rkt 4:1 caadar -- pair
TR opt: derived-pair3.rkt 4:1 caadar -- pair
TR opt: derived-pair3.rkt 4:1 caadar -- pair
TR opt: derived-pair3.rkt 4:1 caadar -- pair
TR opt: derived-pair3.rkt 5:1 caaddr -- pair
TR opt: derived-pair3.rkt 5:1 caaddr -- pair
TR opt: derived-pair3.rkt 5:1 caaddr -- pair
TR opt: derived-pair3.rkt 5:1 caaddr -- pair
TR opt: derived-pair3.rkt 6:1 cadaar -- pair
TR opt: derived-pair3.rkt 6:1 cadaar -- pair
TR opt: derived-pair3.rkt 6:1 cadaar -- pair
TR opt: derived-pair3.rkt 6:1 cadaar -- pair
TR opt: derived-pair3.rkt 7:1 cadadr -- pair
TR opt: derived-pair3.rkt 7:1 cadadr -- pair
TR opt: derived-pair3.rkt 7:1 cadadr -- pair
TR opt: derived-pair3.rkt 7:1 cadadr -- pair
TR opt: derived-pair3.rkt 8:1 caddar -- pair
TR opt: derived-pair3.rkt 8:1 caddar -- pair
TR opt: derived-pair3.rkt 8:1 caddar -- pair
TR opt: derived-pair3.rkt 8:1 caddar -- pair
TR opt: derived-pair3.rkt 9:1 cadddr -- pair
TR opt: derived-pair3.rkt 9:1 cadddr -- pair
TR opt: derived-pair3.rkt 9:1 cadddr -- pair
TR opt: derived-pair3.rkt 9:1 cadddr -- pair
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
2
3
3
4
3
4
4
5

END
#lang typed/racket #:optimize
#reader typed-racket-test/optimizer/reset-port

(caaaar (cons (cons (cons (cons 1 2) 3) 4) 5))
(caaadr (cons 1 (cons (cons (cons 2 3) 4) 5)))
(caadar (cons (cons 1 (cons (cons 2 3) 4)) 5))
(caaddr (cons 1 (cons 2 (cons (cons 3 4) 5))))
(cadaar (cons (cons (cons 1 (cons 2 3)) 4) 5))
(cadadr (cons 1 (cons (cons 2 (cons 3 4)) 5)))
(caddar (cons (cons 1 (cons 2 (cons 3 4))) 5))
(cadddr (cons 1 (cons 2 (cons 3 (cons 4 5)))))
(cdaaar (cons (cons (cons (cons 1 2) 3) 4) 5))
(cdaadr (cons 1 (cons (cons (cons 2 3) 4) 5)))
(cdadar (cons (cons 1 (cons (cons 2 3) 4)) 5))
(cdaddr (cons 1 (cons 2 (cons (cons 3 4) 5))))
(cddaar (cons (cons (cons 1 (cons 2 3)) 4) 5))
(cddadr (cons 1 (cons (cons 2 (cons 3 4)) 5)))
(cdddar (cons (cons 1 (cons 2 (cons 3 4))) 5))
(cddddr (cons 1 (cons 2 (cons 3 (cons 4 5)))))
