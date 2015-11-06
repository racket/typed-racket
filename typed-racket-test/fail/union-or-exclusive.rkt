#;
(exn-pred exn:fail:contract? "Real")
#lang typed/racket #:no-optimize


(module m1 racket
  (define (fix-vector-field-fun f)
    (cond [(procedure-arity-includes? f 2 #t)
           (λ (x y) (f x y))]
          [else
           (λ (x y) (f (vector x y)))]))
  (provide fix-vector-field-fun))

(require/typed
 (submod "." m1)
 [fix-vector-field-fun  (-> (U (-> Real Real Any)
                               (-> (Vector Real Real) Any))
                            (-> Real Real Any))])

(: f : (Vector Real Real) -> (Listof Real))
(define f (λ ([x : (Vector Real Real)] [ignored : Any #f])
            (list (vector-ref x 0) (vector-ref x 1))))

((fix-vector-field-fun f) 0.0 0.0)
