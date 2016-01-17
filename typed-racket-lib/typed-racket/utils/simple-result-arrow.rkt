#lang racket/base
(require racket/unsafe/ops racket/contract/base racket/contract/combinator
         racket/format)

;; An optimized contract combinator for (-> any/c <flat-contract?>)
;; This avoids the extra checks for the `any/c` projection, which
;; can be significant if the function and result contract are both
;; very simple. Also simplifies the code relative to the full power
;; of `->`.

(provide simple-result->)

(define (simple-result-> c)
  (define c* (coerce-flat-contract 'simple-result-> c))
  (define pred (flat-contract-predicate c*))
  (define n (contract-name c*))
  (make-chaperone-contract
   #:name `(-> any/c ,n)
   #:first-order (λ (v) (and (procedure? v) (procedure-arity-includes? v 1)))
   #:late-neg-projection
   (λ (blm)
     (lambda (v neg)
       ;; We could have separate kinda-fast paths for when one of these conditions
       ;; is true, but that is unlikely to be an important case in practice.
       (if (and (equal? 1 (procedure-arity v))
                (equal? 1 (procedure-result-arity v)))
           (unsafe-chaperone-procedure
            v
            (λ (arg) 
              (define res (v arg))
              (unless (with-contract-continuation-mark (cons blm neg) (pred res))
                (raise-blame-error
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
                   blm #f
                   (list 'expected: "one non-keyword argument"
                         'given: (~a (length args) " arguments and " (length vals)
                                     " keyword arguments"))))
             (case-lambda
              [(arg)
               (call-with-values (λ () (v arg))
                 (case-lambda [(res)
                               (unless (with-contract-continuation-mark
                                           (cons blm neg)
                                           (pred res))
                                 (raise-blame-error
                                  blm #f
                                  (list 'expected: (~s n) 'given: (~s res))))
                               res]
                              [results
                               (raise-blame-error 
                                blm results
                                (list 'expected "one value"
                                      'given (~a (length results)
                                                 " values")))]))]
              [args
               (raise-blame-error
                blm #f
                (list 'expected: "one argument"
                      'given: (~a (length args) " arguments")))]))))))))

(module+ test
  (struct m (x))
  (define val (m 1))
  (define c0 (-> any/c real?))
  (define c1 (unconstrained-domain-> real?))
  (define c2 (simple-result-> real?))
  
  (define f0 (contract c0 m-x 'pos 'neg))
  (define f1 (contract c1 m-x 'pos 'neg))
  (define f2 (contract c2 m-x 'pos 'neg))
  (define f3 (contract c2 number->string 'pos 'neg))
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
          (m-x val))))
