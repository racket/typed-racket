#lang typed/racket/base

(require typed/racket/unsafe)


(unsafe-require/typed/provide 
 typed/racket/base
 [make-vector
  (All (A) (-> ([size : Natural]
                [val : A])
               (Refine [v : (Vectorof A)]
                       (= size (vector-length v)))))])


