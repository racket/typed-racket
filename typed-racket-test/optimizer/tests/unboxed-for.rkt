#;#;
#<<END
TR opt: unboxed-for.rkt 2:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- call to fun with unboxed args
TR opt: unboxed-for.rkt 2:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- fun -> unboxed fun
TR opt: unboxed-for.rkt 2:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed call site
TR opt: unboxed-for.rkt 2:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed call site
TR opt: unboxed-for.rkt 2:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed let loop
TR opt: unboxed-for.rkt 2:31 sum -- unbox float-complex
TR opt: unboxed-for.rkt 2:31 sum -- unboxed complex variable
TR opt: unboxed-for.rkt 2:31 sum -- unboxed complex variable
TR opt: unboxed-for.rkt 2:31 sum -- unboxed var -> table
TR opt: unboxed-for.rkt 3:33 (quote (1.0+2.0i 2.0+4.0i)) -- in-list
TR opt: unboxed-for.rkt 4:11 sum -- leave var unboxed
TR opt: unboxed-for.rkt 4:6 (+ i sum) -- unboxed binary float complex
TR opt: unboxed-for.rkt 4:9 i -- unbox float-complex
END
#<<END
3.0+6.0i

END
#lang typed/scheme
#:optimize
#reader typed-racket-test/optimizer/reset-port

(for/fold: : Float-Complex   ((sum : Float-Complex   0.0+0.0i))
           ((i : Float-Complex   '(1.0+2.0i 2.0+4.0i)))
      (+ i sum))
