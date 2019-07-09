#;#;
#<<END
TR info: fixnum.rkt 12:0 (fx* (values (ann x Index)) (values (ann y Index))) -- non-optimized fixnum op
TR info: fixnum.rkt 15:0 (fx+ (+ 300 301) (+ 301 302)) -- non-optimized fixnum op
TR info: fixnum.rkt 16:0 (fxquotient -4 -5) -- non-optimized fixnum op
TR info: fixnum.rkt 42:29 (- (expt 2 (- (system-type (quote word)) (sub1 tag-bits))) 1) -- possible exact real arith
TR info: fixnum.rkt 42:32 (expt 2 (- (system-type (quote word)) (sub1 tag-bits))) -- possible exact real arith
TR info: fixnum.rkt 43:29 (- 1 (expt 2 (- (system-type (quote word)) (sub1 tag-bits)))) -- possible exact real arith
TR info: fixnum.rkt 43:34 (expt 2 (- (system-type (quote word)) (sub1 tag-bits))) -- possible exact real arith
TR info: fixnum.rkt 45:29 (- (expt 2 (- (system-type (quote word)) (sub1 tag-bits))) 2) -- possible exact real arith
TR info: fixnum.rkt 45:32 (expt 2 (- (system-type (quote word)) (sub1 tag-bits))) -- possible exact real arith
TR info: fixnum.rkt 46:29 (- (expt 2 (- (system-type (quote word)) (sub1 tag-bits)))) -- possible exact real arith
TR info: fixnum.rkt 46:32 (expt 2 (- (system-type (quote word)) (sub1 tag-bits))) -- possible exact real arith
TR info: fixnum.rkt 49:28 (- 1 (expt 2 (- (system-type (quote word)) tag-bits))) -- possible exact real arith
TR info: fixnum.rkt 49:33 (expt 2 (- (system-type (quote word)) tag-bits)) -- possible exact real arith
TR info: fixnum.rkt 50:28 (expt 2 (- (system-type (quote word)) tag-bits)) -- possible exact real arith
TR missed opt: fixnum.rkt 10:0 (+ (ann z Fixnum) 234) -- out of fixnum range
TR missed opt: fixnum.rkt 11:0 (* (ann x Index) (ann y Index)) -- out of fixnum range
TR missed opt: fixnum.rkt 14:0 (+ (+ 300 301) (+ 301 302)) -- out of fixnum range
TR missed opt: fixnum.rkt 32:9 (sub1 min-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 38:9 (add1 max-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 42:3 (- max-fixnum min-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 43:3 (- min-fixnum max-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 45:3 (+ max-fixnum max-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 46:3 (+ min-fixnum min-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 50:3 (quotient min-fixnum -1) -- out of fixnum range
TR opt: fixnum.rkt 14:15 (+ 301 302) -- fixnum bounded expr
TR opt: fixnum.rkt 14:3 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum.rkt 15:17 (+ 301 302) -- fixnum bounded expr
TR opt: fixnum.rkt 15:5 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum.rkt 33:9 (add1 min-fixnum) -- fixnum add1
TR opt: fixnum.rkt 35:9 (- max-fixnum) -- unary fixnum
TR opt: fixnum.rkt 36:9 (abs max-fixnum) -- unary number
TR opt: fixnum.rkt 37:9 (sub1 max-fixnum) -- fixnum sub1
TR opt: fixnum.rkt 40:0 (= (- max-fixnum max-fixnum) 0) -- binary fixnum comp
TR opt: fixnum.rkt 40:3 (- max-fixnum max-fixnum) -- fixnum bounded expr
TR opt: fixnum.rkt 41:0 (= (- min-fixnum min-fixnum) 0) -- binary fixnum comp
TR opt: fixnum.rkt 41:3 (- min-fixnum min-fixnum) -- fixnum bounded expr
TR opt: fixnum.rkt 42:63 (sub1 tag-bits) -- fixnum sub1
TR opt: fixnum.rkt 43:65 (sub1 tag-bits) -- fixnum sub1
TR opt: fixnum.rkt 45:63 (sub1 tag-bits) -- fixnum sub1
TR opt: fixnum.rkt 46:63 (sub1 tag-bits) -- fixnum sub1
TR opt: fixnum.rkt 47:0 (= (+ max-fixnum min-fixnum) -1) -- binary fixnum comp
TR opt: fixnum.rkt 47:3 (+ max-fixnum min-fixnum) -- fixnum bounded expr
TR opt: fixnum.rkt 49:3 (quotient max-fixnum -1) -- nonzero fixnum bounded expr
TR opt: fixnum.rkt 7:10 (* x y) -- fixnum bounded expr
END
#<<END
468
234
234
3
1204
1204
0
#f
#f
#f
#t
#t
#t
#t
#f
#t
#t
#t
#t
#t
#t
#t
#t
#t

END
#lang typed/racket
#reader typed-racket-test/optimizer/reset-port

(require racket/fixnum)


(define x 3)
(define y 78)
(define z (* x y)) ; this should be optimized

;; this should not, (+ Fixnum Byte), but it may look like it should
(+ (ann z Fixnum) 234)
(* (ann x Index) (ann y Index))
(fx* (values (ann x Index)) (values (ann y Index))) ; not reported, by design
(abs (ann -3 Fixnum))
(+ (+ 300 301) (+ 301 302))
(fx+ (+ 300 301) (+ 301 302)) ; not reported, by design
(fxquotient -4 -5) ; not reported, by design

(define tag-bits
  (if (eq? (system-type 'vm) 'chez-scheme)
      4
      2))

(: min-fixnum Negative-Fixnum)
(define min-fixnum
  (cast (- (expt 2 (- (system-type 'word) tag-bits))) Negative-Fixnum))
(: max-fixnum Positive-Fixnum)
(define max-fixnum
  (cast (- (expt 2 (- (system-type 'word) tag-bits)) 1) Positive-Fixnum))

(fixnum? (- min-fixnum))
(fixnum? (abs min-fixnum))
(fixnum? (sub1 min-fixnum))
(fixnum? (add1 min-fixnum))

(fixnum? (- max-fixnum))
(fixnum? (abs max-fixnum))
(fixnum? (sub1 max-fixnum))
(fixnum? (add1 max-fixnum))

(= (- max-fixnum max-fixnum) 0)
(= (- min-fixnum min-fixnum) 0)
(= (- max-fixnum min-fixnum) (- (expt 2 (- (system-type 'word) (sub1 tag-bits))) 1))
(= (- min-fixnum max-fixnum) (- 1 (expt 2 (- (system-type 'word) (sub1 tag-bits)))))

(= (+ max-fixnum max-fixnum) (- (expt 2 (- (system-type 'word) (sub1 tag-bits))) 2))
(= (+ min-fixnum min-fixnum) (- (expt 2 (- (system-type 'word) (sub1 tag-bits)))))
(= (+ max-fixnum min-fixnum) -1)

(= (quotient max-fixnum -1) (- 1 (expt 2 (- (system-type 'word) tag-bits))))
(= (quotient min-fixnum -1) (expt 2 (- (system-type 'word) tag-bits)))
