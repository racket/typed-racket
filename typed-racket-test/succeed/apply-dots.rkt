#lang typed-scheme

(plambda: (a ...) ([z : String] . [w : Number ... a])
          (apply (lambda: ([x : Number] . [y : Number ... a]) x)
                 1 w))

(plambda: (a ...) ([z : String] . [w : Number ... a])
          (apply (lambda: ([x : Number] . [y : Number *]) x)
                 1 w))

(plambda: (a ...) ([z : String] . [w : Number *])
          (apply (lambda: ([x : Number] . [y : Number *]) x)
                 1 w))

; the next lambda fails currently because we are incomplete in our
; checking of case-lambdas. Specifically, since it is unannotated
; we check each clause and get an arrow type:
; (-> Number Number ... a Number)
; (-> String String String * Zero)
; (-> Number * Zero)
; and we cannot _naively_ intersect these (i.e. just put them in a
; case->), we would need to intersect them in a more complex way
; that ensured soundness.
;(plambda: (a ...) ([z : String] . [w : Number *])
;  (apply (case-lambda: (([x : Number] . [y : Number ... a]) x)
;                       (([x : String] [y : String] . [z : String *]) 0)
;                       ([y : Number *] 0))
;         w))

;; */*/poly
(plambda: (a ...) ([z : String] . [w : Number *])
          (apply (plambda: (b) ([x : b] . [y : Number *]) x)
                 1 w))

(plambda: (a ...) ([z : String] . [w : Number *])
          (apply (plambda: (b) ([x : b] . [y : Number *]) x)
                 1 2 3 w))

;; */*/polydots
(plambda: (a ...) ([z : String] . [w : Number *])
          (apply (plambda: (b ...) ([x : Number] . [y : Number *]) x)
                 1 w))

(plambda: (a ...) ([z : String] . [w : Number *])
          (apply (plambda: (b ...) ([x : Number] . [y : Number *]) x)
                 1 1 1 w))

;; */.../poly
(plambda: (a ...) ([z : String] . [w : Number ... a])
          (apply (plambda: (b) ([x : Number] . [y : Number *]) x)
                 1 w))

(plambda: (a ...) ([z : String] . [w : Number ... a])
          (apply (plambda: (b) ([x : Number] . [y : Number *]) x)
                 1 1 1 1 w))

;; */.../polydots
#;(plambda: (a ...) ([z : String] . [w : Number ... a])
          (apply (plambda: (b ...) ([x : Number] . [y : Number *]) x)
                 1 w))

#;(plambda: (a ...) ([z : String] . [w : Number ... a])
          (apply (plambda: (b ...) ([x : Number] . [y : Number *]) x)
                 1 1 1 1 w))

;; .../.../poly
(plambda: (a ...) ([z : String] . [w : Number ... a])
          (apply (plambda: (b) ([x : Number] . [y : Number ... a]) x)
                 1 w))

#;(plambda: (a ...) ([z : String] . [w : Number ... a])
          (apply (plambda: (b ...) ([x : Number] . [y : Number ... a]) x)
                 1 w))

