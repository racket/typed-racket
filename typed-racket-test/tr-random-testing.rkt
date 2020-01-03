#lang racket

;; Random testing of the TR numeric base environment, and numeric optimizations

(require redex/reduction-semantics
         racket/flonum racket/unsafe/ops
         racket/runtime-path racket/sandbox racket/cmdline
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

;; this works around racket/racket#2865
(define (safe-arithmetic-shift a b)
  (if (> b 100000) b (arithmetic-shift a b)))

(define-language tr-arith
  [n real]
  ;; randomly generate F, not E, because literal numbers self-evaluate
  ;; (i.e. generates a useless test)
  [E* n E F I]
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
     (real->double-flonum F*)
     (exact->inexact F*)
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
     (safe-arithmetic-shift I* I*)
     (bitwise-and I* ...)
     (bitwise-ior I* ...)
     (bitwise-xor I* ...)
     (bitwise-not I*)
     (integer-length I*)
     (inexact->exact I*)
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
  [C (let ([x R] [y R]) (if (INEQ x y) x  #f))
     (let ([x R] [y R]) (if (INEQ x y) #f x))
     (let ([x R] [y R]) (if (INEQ x y) y #f))
     (let ([x R] [y R]) (if (INEQ x y) #f y))])
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
               0)) ; arbitrary
          racketcs-available?)] ; if so, don't generate single-flonums
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

(define (right-type? before [verbose? #f])
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

(define (reset-evals!)
  (set! racket-eval (mk-eval 'racket))
  (set! tr-eval (mk-eval 'typed/racket)))


(define-values (bin-dir _1 _2)
  (split-path (find-executable-path (find-system-path 'exec-file))))
(define racketcs (build-path bin-dir "racketcs"))
(define-runtime-path racketcs-harness "./racketcs-eval-server.rkt")
(define-values (rcs-process rcs-out rcs-in rcs-err)
  (cond [(file-exists? racketcs)
         (subprocess #f #f #f racketcs racketcs-harness)]
        [else
         (eprintf "WARNING: did not find racketcs executable\n")
         (values #f #f #f #f)]))
(define racketcs-available? (and rcs-process #t))

(define (racketcs-eval sexp)
  (writeln sexp rcs-in)
  (flush-output rcs-in)
  (define result (read rcs-out))
  (match result
    [(? pair?)       (error 'racketcs-eval (~a result))]
    [(vector r s) #;(unless (equal? s sexp)
                    (printf "not equal!!! \n~s \n~s\n" s sexp))
                  r]))

(define (simplify-expanded e)
  (match e
    [`(#%app . ,e) (map simplify-expanded e)]
    [`(quote ,(? number? n)) n]
    [(? list?) (map simplify-expanded e)]
    [e e]))

(define (same-result-as-untyped? sexp [verbose? #f])
  (define racket-failed?  #f)
  (define both-failed?    #f)
  (define racket-result
    (with-handlers
        ;; something went wrong, e.g. division by zero
        ;; TR must fail too
      ([exn? (λ (e)
               (when (exn:fail:resource? e)
                 (reset-evals!))
               (set! racket-failed? #t))])
      (racket-eval sexp)))
  (define tr-result
    (with-handlers
        ;; did Racket error too?
      ([exn? (λ (e)
               (when (exn:fail:resource? e)
                 (reset-evals!))
               (when racket-failed?
                 (set! both-failed? #t))
               e)])
      (tr-eval sexp)))
  (or both-failed?
      (and (not racket-failed?)
           ;; for NaN, which is not = to itself
           (or (equal? racket-result tr-result)
               (begin (printf "not same result untyped: ~s typed: ~s\n" racket-result tr-result)
                      #;(printf "expanded typed code is: ~s\n"
                              (simplify-expanded (tr-eval `(syntax->datum (expand (quote (#%top-interaction . ,sexp)))))))
                      #f)))))

(define (same-result? a b)
  (or (equal? a b)
      ;; ok for inexact numbers to differ by a small amount (RacketCS is often more precise)
      ;; FIXME: this doesn't handle cases where we get inf or nan on one side
      (and (real? a) (real? b) (inexact? a) (inexact? b)
           (< (abs (- a b)) (/ b 1000000)))
      ;; racketcs doesn't have single flonums
      (and (single-flonum? a) (flonum? b)
           (< (abs (- (real->double-flonum b))) (/ b 1000000)))
      (and (eq? a +nan.f) (eq? b +nan.0))
      (and (eq? a +inf.f) (eq? b +inf.0))
      (and (eq? a -inf.f) (eq? b -inf.0))
      ;; same for complex inexact
      (and (complex? a) (complex? b) (not (real? a)) (not (real? b))
           (same-result? (real-part a) (real-part b))
           (same-result? (imag-part a) (imag-part b)))
      ;; RacketCS does not have mixed-exactness complex numbers
      (and (complex? a) (complex? b) (not (real? a)) (not (real? b))
           (inexact? (real-part b)) (inexact? (imag-part b))
           (equal? (exact->inexact (real-part a)) (real-part b))
           (equal? (exact->inexact (imag-part a)) (imag-part b)))))

(define (contains-real->single-flonum? s)
  (let loop ([s s])
    (match s
      ['real->single-flonum #t]
      [(cons a b) (or (loop a) (loop b))]
      [_ #f])))

(define (same-result-as-racketcs? sexp [verbose? #f])
  (define racket-failed?   #f)
  (define both-failed?     #f)
  (define racket-result
    (with-handlers ([exn? (λ (e) (set! racket-failed? #t))])
      (racket-eval sexp)))
  (define racketcs-result
    (with-handlers ([exn:fail:filesystem:errno?
                     (λ (e)
                       ;; intermittently ends up with a broken pipe between us
                       ;; and the racketcs server. I have not been able to
                       ;; narrow it down
                       ;; TODO instead, could pre-emptively restart racketcs
                       ;;   process every ~200 tests
                       (eprintf "broken pipe, aborting\n")
                       (raise e))] ; just give up on the rest of this run
                    [(lambda (e) (regexp-match "real->single-flonum: unsupported" (exn-message e)))
                     ;; ignore this test case
                     (lambda _ (set! both-failed? #t))]
                    [exn? (λ (e) (if racket-failed?
                                     (set! both-failed? #t)
                                     e))])
      (racketcs-eval sexp)
      #;
      (if (contains-real->single-flonum? sexp)
          (begin (printf "contains real->single-flonum ~s\n" sexp)
                 (set! both-failed? #t))
          )))
  (or both-failed?
      (and (not racket-failed?)
           (if (same-result? racket-result racketcs-result)
               #t
               (begin (printf "not same as cs: racket: ~s racketcs: ~s\n"
                              racket-result racketcs-result)
                      #f)))))

(define num-exceptions 0)

(define (check-all-reals sexp [verbose? #f])
  ;; because some of the generated expressions compute gigantic bignums, running
  ;; out of resources is expected, so just ignore that case
  (with-handlers ([exn:fail:resource? (lambda (e) (reset-evals!) e)])
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
         (and (right-type? sexp verbose?)
              (same-result-as-untyped? sexp verbose?)
              (if #f #;racketcs-available?
                  (same-result-as-racketcs? sexp verbose?)
                  #t))))))

(define (run-tests)
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

(module+ main
  (run-tests))

(module+ test
  (module config info
    (define timeout 600)
    (define random? #t))
  (run-tests))
