#;#;
#<<END
TR opt: extflonums-cs-skip-all.rkt 10:11 (extfl- 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums-cs-skip-all.rkt 11:11 (extfl* 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums-cs-skip-all.rkt 12:11 (extfl/ 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums-cs-skip-all.rkt 13:6 (extfl= 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums-cs-skip-all.rkt 14:6 (extfl< 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums-cs-skip-all.rkt 15:6 (extfl> 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums-cs-skip-all.rkt 16:6 (extfl<= 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums-cs-skip-all.rkt 17:6 (extfl>= 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums-cs-skip-all.rkt 18:6 (extflmin 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums-cs-skip-all.rkt 19:6 (extflmax 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums-cs-skip-all.rkt 20:6 (extflexpt 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums-cs-skip-all.rkt 22:11 (extflabs 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 23:11 (extflround 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 24:11 (extflfloor 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 25:11 (extflceiling 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 26:11 (extfltruncate 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 27:11 (extflsin 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 28:11 (extflcos 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 29:11 (extfltan 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 30:11 (extflasin 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 31:11 (extflacos 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 32:11 (extflatan 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 33:11 (extfllog 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 34:11 (extflexp 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 35:11 (extflsqrt 1.0t0) -- unary extflonum
TR opt: extflonums-cs-skip-all.rkt 37:11 (->extfl (bitwise-and 1 2)) -- fixnum to extflonum conversion
TR opt: extflonums-cs-skip-all.rkt 37:20 (bitwise-and 1 2) -- binary fixnum
TR opt: extflonums-cs-skip-all.rkt 38:11 (real->extfl (bitwise-and 1 2)) -- fixnum to extflonum conversion
TR opt: extflonums-cs-skip-all.rkt 38:24 (bitwise-and 1 2) -- binary fixnum
TR opt: extflonums-cs-skip-all.rkt 9:11 (extfl+ 1.0t0 1.0t0) -- binary extflonum
END
#<<END
"2.00000"
"0.00000"
"1.00000"
"1.00000"
#t
#f
#f
#t
#t
1.0t0
1.0t0
1.0t0
"1.00000"
"1.00000"
"1.00000"
"1.00000"
"1.00000"
"0.84147"
"0.54030"
"1.55741"
"1.57080"
"0.00000"
"0.78540"
"0.00000"
"2.71828"
"1.00000"
"0.00000"
"0.00000"

END

#lang typed/racket
#reader typed-racket-test/optimizer/reset-port

(require racket/extflonum)

;; to avoid machine-specific precision issues
(: out : ExtFlonum -> String)
(define (out v) (real->decimal-string (extfl->inexact v) 5))
(define-syntax-rule (wrap e)
  (if (extflonum-available?) e (void)))
(wrap (out (extfl+ 1.0t0 1.0t0)))
(wrap (out (extfl- 1.0t0 1.0t0)))
(wrap (out (extfl* 1.0t0 1.0t0)))
(wrap (out (extfl/ 1.0t0 1.0t0)))
(wrap (extfl= 1.0t0 1.0t0))
(wrap (extfl< 1.0t0 1.0t0))
(wrap (extfl> 1.0t0 1.0t0))
(wrap (extfl<= 1.0t0 1.0t0))
(wrap (extfl>= 1.0t0 1.0t0))
(wrap (extflmin 1.0t0 1.0t0))
(wrap (extflmax 1.0t0 1.0t0))
(wrap (extflexpt 1.0t0 1.0t0))

(wrap (out (extflabs 1.0t0)))
(wrap (out (extflround 1.0t0)))
(wrap (out (extflfloor 1.0t0)))
(wrap (out (extflceiling 1.0t0)))
(wrap (out (extfltruncate 1.0t0)))
(wrap (out (extflsin 1.0t0)))
(wrap (out (extflcos 1.0t0)))
(wrap (out (extfltan 1.0t0)))
(wrap (out (extflasin 1.0t0)))
(wrap (out (extflacos 1.0t0)))
(wrap (out (extflatan 1.0t0)))
(wrap (out (extfllog 1.0t0)))
(wrap (out (extflexp 1.0t0)))
(wrap (out (extflsqrt 1.0t0)))

(wrap (out (->extfl (bitwise-and 1 2))))
(wrap (out (real->extfl (bitwise-and 1 2))))
