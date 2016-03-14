#lang racket/base
(require racket/unsafe/ops racket/contract/base racket/contract/combinator
         racket/format racket/private/performance-hint)

;; An optimized contract combinator for (-> any/c <flat-contract?>)
;; This avoids the extra checks for the `any/c` projection, which
;; can be significant if the function and result contract are both
;; very simple. Also simplifies the code relative to the full power
;; of `->`.

(provide simple-result->)

(define-syntax-rule (the-contract n pred arity arg ...)
  (make-chaperone-contract
   #:name `(-> ,@(for/list ([i arity]) 'any/c) ,n)
   #:first-order (λ (v) (and (procedure? v) (procedure-arity-includes? v arity)))
   #:late-neg-projection
   (λ (blm)
     (lambda (v neg)
       ;; We could have separate kinda-fast paths for when one of these conditions
       ;; is true, but that is unlikely to be an important case in practice.
       (if (and (equal? arity (procedure-arity v))
                (equal? 1 (procedure-result-arity v)))
           (unsafe-chaperone-procedure
            v
            (λ (arg ...) 
              (define res (v arg ...))
              (unless (with-contract-continuation-mark (cons blm neg) (pred res))
                (raise-blame-error
                 #:missing-party neg
                 blm #f
                 (list 'expected: (~s n) 'given: (~s res))))
              res))
           (unsafe-chaperone-procedure
            v
            ;; use `make-keyword-procedure` to cover cases
            ;; where a keyword-accepting procedure is imported with a type that
            ;; doesn't mention the keywords
            (make-keyword-procedure
             (λ (kws vals . args) (raise-blame-error
                                   #:missing-party neg
                                   blm #f
                                   (list 'expected: "one non-keyword argument"
                                         'given: (~a (length args) " arguments and " (length vals)
                                                     " keyword arguments"))))
             (case-lambda
               [(arg ...)
                (call-with-values (λ () (v arg ...))
                                  (case-lambda [(res)
                                                (unless (with-contract-continuation-mark
                                                         (cons blm neg)
                                                         (pred res))
                                                  (raise-blame-error
                                                   #:missing-party neg
                                                   blm #f
                                                   (list 'expected: (~s n) 'given: (~s res))))
                                                res]
                                               [results
                                                (raise-blame-error
                                                 #:missing-party neg
                                                 blm results
                                                 (list 'expected "one value"
                                                       'given (~a (length results)
                                                                  " values")))]))]
               [args
                (raise-blame-error
                 #:missing-party neg
                 blm #f
                 (list 'expected: "one argument"
                       'given: (~a (length args) " arguments")))]))))))))

;; arity is how many any/c arguments the function expects
(begin-encourage-inline
  (define (simple-result-> c arity)
    (define c* (coerce-flat-contract 'simple-result-> c))
    (define pred (flat-contract-predicate c*))
    (define n (contract-name c*))
    (case arity
      [(0) (the-contract n pred 0)]
      [(1) (the-contract n pred 1 arg)]
      [(2) (the-contract n pred 2 arg1 arg2)]
      [(3) (the-contract n pred 3 arg1 arg2 arg3)]
      [else (raise-argument-error 'simple-result-> "arity 0, 1, 2, or 3" arity)])))

(module+ test
  (struct m (x))
  (define val (m 1))

  (define thunk (λ () 1))

  
  (define c0 (-> any/c real?))
  (define c1 (unconstrained-domain-> real?))
  (define c2 (simple-result-> real? 1))

  (define d0 (-> real?))
  (define d1 (unconstrained-domain-> real?))
  (define d2 (simple-result-> real? 0))
  
  (define f0 (contract c0 m-x 'pos 'neg))
  (define f1 (contract c1 m-x 'pos 'neg))
  (define f2 (contract c2 m-x 'pos 'neg))
  (define f3 (contract c2 number->string 'pos 'neg))

  (define g0 (contract d0 thunk 'pos 'neg))
  (define g1 (contract d1 thunk 'pos 'neg))
  (define g2 (contract d2 thunk 'pos 'neg))
  
  
  (define N 1000000)
  (collect-garbage)
  'f0
  (time (for/sum ([i (in-range N)])
          (f0 val)))
  (collect-garbage)
  'f1
  (time (for/sum ([i (in-range N)])
          (f1 val)))
  (collect-garbage)
  'f2
  (time (for/sum ([i (in-range N)])
          (f2 val)))

  'm-x
  (time (for/sum ([i (in-range N)])
          (m-x val)))

  (collect-garbage)
  'g0
  (time (for/sum ([i (in-range N)])
          (g0)))
  (collect-garbage)
  'g1
  (time (for/sum ([i (in-range N)])
          (g1)))
  (collect-garbage)
  'g2
  (time (for/sum ([i (in-range N)])
          (g2)))
  'thunk
  (time (for/sum ([i (in-range N)])
          (thunk))))
