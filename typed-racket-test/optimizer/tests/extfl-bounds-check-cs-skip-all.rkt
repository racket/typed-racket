#;#;
#<<END
TR info: extfl-bounds-check-cs-skip-all.rkt 21:11 displayln -- hidden parameter
TR info: extfl-bounds-check-cs-skip-all.rkt 23:11 displayln -- hidden parameter
TR info: extfl-bounds-check-cs-skip-all.rkt 25:11 displayln -- hidden parameter
TR opt: extfl-bounds-check-cs-skip-all.rkt 12:2 (extflvector-ref v i) -- extflvector partial bounds checking elimination
TR opt: extfl-bounds-check-cs-skip-all.rkt 15:2 (extflvector-set! v i n) -- extflvector partial bounds checking elimination
TR opt: extfl-bounds-check-cs-skip-all.rkt 27:10 (extflvector-length efv) -- extflvector-length
TR opt: extfl-bounds-check-cs-skip-all.rkt 6:2 (extflvector-ref v i) -- extflvector partial bounds checking elimination
TR opt: extfl-bounds-check-cs-skip-all.rkt 9:2 (extflvector-set! v i n) -- extflvector partial bounds checking elimination
END
#<<END
3.0t0
4.0t0
5.0t0
4

END

#lang typed/racket
#reader typed-racket-test/optimizer/reset-port
(require racket/flonum racket/extflonum (for-syntax racket/extflonum))


(: b (ExtFlVector Fixnum -> ExtFlonum))
(define (b v i)
  (extflvector-ref v i))
(: c (ExtFlVector Fixnum ExtFlonum -> Void))
(define (c v i n)
  (extflvector-set! v i n))
(: d (ExtFlVector Index -> ExtFlonum))
(define (d v i)
  (extflvector-ref v i))
(: e (ExtFlVector Index ExtFlonum -> Void))
(define (e v i n)
  (extflvector-set! v i n))

(define-syntax (go stx)
  (if (extflonum-available?)
      #'(begin
          (define: efv : ExtFlVector (extflvector 1.0t0 2.0t0 3.0t0 4.0t0))
          (displayln (b efv 2))
          (c efv 2 4.0t0)
          (displayln (d efv 2))
          (e efv 2 5.0t0)
          (displayln (b efv 2))

          (extflvector-length efv))
      #'(begin)))
(go)
