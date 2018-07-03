#lang racket

;; Random testing of the TR numeric base environment, and numeric optimizations

(require redex/reduction-semantics
         racket/flonum racket/unsafe/ops
         racket/sandbox racket/cmdline
         "random-real.rkt")

(require typed-racket/utils/utils
         (typecheck typechecker)
         (utils tc-utils)
         (types subtype utils))

(require (prefix-in b: (base-env base-env))
         (prefix-in n: (base-env base-env-numeric)))

(provide check-all-reals)

(b:init) (n:init)
(define-namespace-anchor anch)

(define-language tr-arith
  [n real]
  ;; randomly generate F, not E, because literal numbers self-evaluate
  ;; (i.e. generates a useless test)
  [E* n E F S I]
  ;; racket/math
  ;; [E (degrees->radians E)
  ;;    (radians->degrees E)
  ;;    (exact-round E)
  ;;    (exact-floor E)
  ;;    (exact-ceiling E)
  ;;    (exact-truncate E)
  ;;    (nan? E)
  ;;    (infinite? E)
  ;;    ]
  ;; more likely to be floats
  [F* (real->double-flonum n) F] ; TODO fix pre-processing to avoid cast
  [F (* F* ...)
     (+ F* ...)
     (- F* ...)
     (/ F* ...)
     (max F* ...)
     (min F* ...)
     (add1 F*)
     (sub1 F*)
     (abs F*)
     (floor F*)
     (ceiling F*)
     (truncate F*)
     (round F*)
     (cos F*)
     (sin F*)
     (tan F*)
     (sqr F*)
     (flabs F*)
     (flround F*)
     (flfloor F*)
     (flceiling F*)
     (fltruncate F*)
     (flsin F*)
     (flcos F*)
     (fltan F*)
     (flatan F*)
     (flasin F*)
     (flacos F*)
     (fllog F*)
     (flexp F*)
     (flsqrt F*)
     (unsafe-flabs F*)
     (unsafe-flmin F* F*)
     (unsafe-flmax F* F*)
     (unsafe-flsqrt F*)
     (fl+ F* F*)
     (fl- F* F*)
     (fl* F* F*)
     (fl/ F* F*)
     (flmin F* F*)
     (flmax F* F*)
     (flexpt F* F*)
     (unsafe-fl+ F* F*)
     (unsafe-fl- F* F*)
     (unsafe-fl* F* F*)
     (unsafe-fl/ F* F*)]
  ;; not many single-flonum-specific ops, so will mostly be used in E context
  [S (real->single-flonum n)
     (inexact->exact S)
     (real->double-flonum S)]
  ;; more likely to be integers
  [I* (exact-round n) I] ; TODO fix pre-processing to avoid cast
  [I (* I* ...)
     (+ I* ...)
     (- I* ...)
     (max I* ...)
     (min I* ...)
     (add1 I*)
     (sub1 I*)
     (abs I*)
     (floor I*)
     (ceiling I*)
     (truncate I*)
     (round I*)
     (sqr I*)
     (modulo I* I*)
     (remainder I* I*)
     (quotient I* I*)
     (gcd I*)
     (gcd I* I*)
     (gcd I* I* I*)
     (lcm I*)
     (lcm I* I*)
     (lcm I* I* I*)
     (arithmetic-shift I* I*)
     (bitwise-and I* ...)
     (bitwise-ior I* ...)
     (bitwise-xor I* ...)
     (bitwise-not I*)
     (integer-length I*)
     ]
  [E (* E* ...)
     (+ E* ...)
     (- E* ...)
     (/ E* ...)
     (max E* ...)
     (min E* ...)
     (add1 E*)
     (sub1 E*)
     (abs E*)
     (floor E*)
     (ceiling E*)
     (truncate E*)
     (round E*)
     (sqrt E*)
     (log E*)
     (exp E*)
     (cos E*)
     (sin E*)
     (tan E*)
     (sqr E*)
     (make-rectangular E* E*)
     (make-polar E* E*)
     (sinh E*)
     (cosh E*)
     (tanh E*)
     (expt E* E*)]
  [INEQ < <= >= >]
  [R n (+ R R) (- R R) (* R R) (expt R R)]
  [C (let* ([x R] [y R])
       (if (INEQ x y) x #false))
     (let* ([x R] [y R])
       (if (INEQ x y) #false x))
     (let* ([x R] [y R])
       (if (INEQ x y) y #false))
     (let* ([x R] [y R])
       (if (INEQ x y) #false y))])
;; initial version generated from: (map car (file->list "base-env-parts"))

;; Redex can't generate reals, so we convert ints to reals.
(define (exp->real-exp E) ; numbers or symbols or lists
  (cond [(number? E)
         (random-integer->random-real
          (exact-round
           ;; doesn't handle non-rationals
           ;; not a problem, we generate those specially
           (if (rational? E)
               E
               0)))] ; arbitrary
        [(list? E)
         (map exp->real-exp E)]
        [else
         E]))


;; big-step preservation: is the type of the result consistent with the type
;; predicted by the typechecker

(define (get-type e [typecheck (compose tc-expr expand)])
  (parameterize ([delay-errors? #f]
                 [current-namespace (namespace-anchor->namespace anch)]
                 [orig-module-stx (quote-syntax e)])
    (typecheck (datum->syntax #'here e))))

(define (right-type? before)
  (define type-before (match (get-type before) [(tc-result1: b) b]))
  (define after (with-handlers ([values values])
                  (eval before (namespace-anchor->namespace anch))))
  (cond [(exn? after)  #t]
        [else
         (define type-after (get-type after tc-literal))
         (define subtype? (subtype type-after type-before))
         (unless subtype?
           (printf "type-before = ~v~ntype-after = ~v~n" type-before type-after))
         subtype?]))


;; do we get the same result with and without optimization?

(define max-mem 1000)
(define (mk-eval lang)
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize ([sandbox-memory-limit max-mem])
       (make-evaluator lang #:requires '(racket/flonum racket/unsafe/ops))))))
(define racket-eval (mk-eval 'racket))
(define tr-eval     (mk-eval 'typed/racket))

(define (same-result-as-untyped? sexp)
  (define racket-failed?  #f)
  (define both-failed?    #f)
  (define racket-result
    (with-handlers
        ;; something went wrong, e.g. division by zero
        ;; TR must fail too
        ([exn? (λ (e) (set! racket-failed? #t))])
      (racket-eval sexp)))
  (define tr-result
    (with-handlers
        ;; did Racket error too?
        ([exn? (λ (e) (when racket-failed?
                        (set! both-failed? #t)))])
      (tr-eval sexp)))
  (or both-failed?
      (and (not racket-failed?)
           ;; for NaN, which is not = to itself
           (equal? racket-result tr-result))))


(define num-exceptions 0)

(define (check-all-reals sexp [verbose? #f])
  ;; because some of the generated expressions comute gigantic bignums, running
  ;; out of resources is expected, so just ignore that case
  (with-handlers ([exn:fail:resource? values])
    (with-limits
     5 max-mem
     (when verbose? (displayln sexp))
     (or (with-handlers ; attempt to typecheck
             ;; something went wrong, almost certainly typechecking failed
             ;; in which case we ignore the expression
             ([exn?  (λ (e)
                       (set! num-exceptions (+ num-exceptions 1))
                       #t)])
           (get-type sexp)
           #f) ; go on and check properties
         (and (right-type? sexp)
              (same-result-as-untyped? sexp))))))

(module+ main
  (define n-attempts 1000)
  (define seed       (+ 1 (random (expt 2 30))))
  (define verbose?   #f)
  (command-line
   #:once-each
   [("-n") n "Number of attempts" (set! n-attempts (string->number n))]
   [("-s") s "RNG seed"           (set! seed (string->number s))]
   [("-v")   "Print test cases"   (set! verbose? #t)])

  (random-seed seed)
  (printf "seed: ~s~n" seed)
  (flush-output) ; DrDr doesn't print the above if the testing segfaults.

  ;; start with 1000 small, deterministic test cases, to catch regressions
  (redex-check tr-arith E #:in-order (check-all-reals (term E) verbose?)
               #:attempts 1000
               #:prepare exp->real-exp
               #:keep-going? #t)
  (redex-check tr-arith C #:in-order (check-all-reals (term C) verbose?)
               #:attempts 1000
               #:prepare exp->real-exp
               #:keep-going? #t)
  ;; then switch to purely random to get different ones every run
  (redex-check tr-arith E #:ad-hoc (check-all-reals (term E) verbose?)
               #:attempts n-attempts
               #:prepare exp->real-exp
               #:keep-going? #t)
  (redex-check tr-arith C #:ad-hoc (check-all-reals (term C) verbose?)
               #:attempts n-attempts
               #:prepare exp->real-exp
               #:keep-going? #t)

  (printf "bad tests (usually typechecking failed): ~v~n" num-exceptions))

(module+ test
  (module config info
    (define timeout 600)
    (define random? #t)))
