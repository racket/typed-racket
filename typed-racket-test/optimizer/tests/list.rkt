#;#;
#<<END
TR opt: list.rkt 3:1 first -- pair
TR opt: list.rkt 4:1 rest -- pair
TR opt: list.rkt 5:1 second -- pair
TR opt: list.rkt 5:1 second -- pair
TR opt: list.rkt 6:1 rest -- pair
TR opt: list.rkt 7:1 third -- pair
TR opt: list.rkt 7:1 third -- pair
TR opt: list.rkt 7:1 third -- pair
TR opt: list.rkt 8:1 fourth -- pair
TR opt: list.rkt 8:1 fourth -- pair
TR opt: list.rkt 8:1 fourth -- pair
TR opt: list.rkt 8:1 fourth -- pair
END
#<<END
1
'(2 3 4)
2
'(3 4)
3
4

END
#lang typed/racket
#reader typed-racket-test/optimizer/reset-port

(define: l : (List Integer Integer Integer Integer) '(1 2 3 4))
(first l)
(rest l)
(second l)
(rest (rest l))
(third l)
(fourth l)
