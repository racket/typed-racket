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

