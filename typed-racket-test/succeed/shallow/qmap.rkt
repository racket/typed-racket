#lang typed/racket/base/shallow

;; TODO parse error in type; used a type variable not bound with ... as a bound on a ...

(require
  racket/list)

(define-type (Stream A)
  (Rec Stream (U Null (Boxof (U (-> (Pair A Stream))
                                (Pair A Stream))))))

(struct: (A) Queue ([front : (Stream A)]
                    [rear  : (Listof A)]
                    [scdul : (Stream A)]))

;; similar to list map function. apply is expensive so using case-lambda
;; in order to saperate the more common case
(: qmap : 
   (All (A C B ...) 
        (case-lambda 
          ((A -> C) (Queue A) -> Void)
          ((A B ... B -> C) (Queue A) (Queue B) ... B -> Void))))
(define qmap
  (pcase-lambda: (A C B ...)
                 [([func : (A -> C)]
                   [deq  : (Queue A)])
                  (void)]
                 [([func : (A B ... B -> C)]
                   [deq  : (Queue A)] . [deqs : (Queue B) ... B])
                  (void)]))

