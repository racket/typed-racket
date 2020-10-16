#lang racket/base

(require rackunit rackunit/text-ui racket/file
         racket/port rackunit/log
         racket/string
         compiler/compiler setup/setup racket/promise
         racket/match syntax/modcode
         racket/promise racket/runtime-path
         "unit-tests/all-tests.rkt"
         "unit-tests/test-utils.rkt"
         "optimizer/run.rkt"
         "places.rkt" "send-places.rkt")

(define (scheme-file? s)
  (regexp-match ".*[.](rkt|ss|scm)$" (path->string s)))

(define-namespace-anchor a)

(define (exn-matches . args)
  (values
   (lambda (val)
     (and (exn? val)
          (for/and ([e args])
                   (cond [(procedure? e) (e val)]
                         [(number? e)
                          (and (exn:fail:syntax? val)
                               (= e (length (exn:fail:syntax-exprs val))))]
                         [(or (string? e) (regexp? e))
                          (regexp-match e (exn-message val))]
                         [else (error 'exn-pred "bad argument" e)]))))
   args))


(define (exn-pred p)
  (let ([sexp (with-handlers
                  ([exn:fail? (lambda _ #f)])
                (call-with-input-file*
                 p
                 (lambda (prt)
                   (read-line prt 'any) (read prt))))])
    (match sexp
      [(list-rest 'exn-pred e)
       (eval `(exn-matches . ,e) (namespace-anchor->namespace a))]
      [_
       (exn-matches ".*Type Checker.*" exn:fail:syntax?)])))

(define-runtime-path src-dir ".")

(define (mk-tests dir test #:error [error? #f] #:exclude [excl ""] )
  (lambda ()
    (define path (build-path src-dir dir))
    (define prms
      (for/list ([i (in-naturals)]
                 [p (directory-list path)]
                 #:when (scheme-file? p)
                 #:unless (and (not (equal? excl "")) (string-contains? (path->string p) excl))
		 ;; skip backup files
		 #:when (not (regexp-match #rx".*~" (path->string p))))
        (define p* (build-path path p))
        (define prm (list path p 
                          (if (places)
                              (delay/thread
                                (begin0 (run-in-other-place p* error?)
                                        (when (zero? (modulo i 10))
                                          (eprintf "."))))
                              (delay 
                                (parameterize ([read-accept-reader #t]
                                               [current-load-relative-directory path]
                                               [current-directory path]
                                               [current-output-port (open-output-nowhere)])
                                  (begin0 (dr p)
                                          (when (zero? (modulo i 10))
                                            (eprintf "."))))))))
        prm))
    (define tests
      (for/list ([e prms])
        (match-define (list path p prm) e)
        (test-suite
         (path->string p)
         (test
          (build-path path p)
          (λ ()
            (when (verbose?)
              (log-warning (format "TR tests: waiting for ~a ~a" dir p)))
            (force prm))))))
    (make-test-suite dir tests)))



(define (int-tests [excl ""])
  (define succ-tests (mk-tests "succeed"
                             (lambda (p thnk) 
                               (check-not-exn thnk))
                             #:exclude excl))
  (define fail-tests (mk-tests "fail"
                             (lambda (p thnk)
                               (define-values (pred info) (exn-pred p))
                               (parameterize ([error-display-handler void])
                                 (with-check-info
                                  (['predicates info])
                                  (check-exn pred thnk))))
                             #:error #t))
  
  (test-suite "Integration tests"
              (succ-tests)
              (fail-tests)))

(define (compile-benchmarks)
  (define shootout (collection-path "tests" "racket" "benchmarks" "shootout" "typed"))
  (define common (collection-path "tests" "racket" "benchmarks" "common" "typed"))
  (define (mk dir)
    (let ((promised-results
            (for/hash ([file (in-list (directory-list dir))]
                        #:when (scheme-file? file))
              (values (path->string file)
                      (delay/thread (compile-path (build-path dir file)))))))
      (make-test-suite (path->string dir)
        (for/list ([(name results) promised-results])
           (test-suite name
              (check-not-exn (λ () (force results))))))))


  (test-suite "Compiling Benchmark tests"
              (mk shootout)
              (mk common)))

(define (compile-math)
  (test-suite "Compiling Math library"
              (check-true
               (parameterize ([current-output-port (open-output-nowhere)])
                 (setup #:collections '(("math")))))))


(define (just-one p*)
  (define-values (path p b) (split-path p*))
  (define f
    (let ([dir (path->string path)])
      (cond [(regexp-match? #rx"fail/$" dir )
             (lambda (p thnk)
               (define-values (pred info) (exn-pred p))
               (parameterize ([error-display-handler void])
                 (with-check-info
                  (['predicates info])
                  (check-exn pred thnk))))]
            [(regexp-match? #rx"succeed/$" dir)
             (lambda (p thnk) (check-not-exn thnk))]
            [(regexp-match? #rx"optimizer/tests/$" dir)
             (lambda (p* thnk) (test-opt p))]
            [(regexp-match? #rx"optimizer/missed-optimizations/$" dir)
             (lambda (p* thnk) (test-missed-optimization p))]
            [else
              (error 'just-one "Unknown test kind for test: ~a" p*)])))
  (test-suite
   (path->string p)
   (f
    (build-path path p)
    (lambda ()
      (parameterize ([read-accept-reader #t]
                     [current-load-relative-directory
                      (path->complete-path path)]
                     [current-directory path]
                     [current-output-port (open-output-nowhere)])
        (dr p))))))


(define (test/gui suite)
  (((dynamic-require 'rackunit/private/gui/gui 'make-gui-runner))
   suite))


(define (go tests) (test/gui tests))
(define (go/text tests)
  (force (delay/thread (run-tests tests 'verbose))))

(provide go go/text just-one places start-workers
         verbose?
         int-tests unit-tests compile-benchmarks compile-math
         optimization-tests missed-optimization-tests)

(module+ main
  (require racket/vector racket/gui/dynamic rackunit racket/cmdline)

  (define exec (make-parameter go/text))
  (define nightly? (make-parameter #f))
  (define unit? (make-parameter #f))
  (define int? (make-parameter #f))
  (define opt? (make-parameter #f))
  (define missed-opt? (make-parameter #f))
  (define bench? (make-parameter #f))
  (define math? (make-parameter #f))
  (define excl (make-parameter ""))
  (define single (make-parameter #f))
  (current-namespace (make-base-namespace))
  (command-line
   #:once-each
   ["-v" "verbose" (verbose? #t)]
   ["--unit" "run the unit tests" (unit? #t)]
   ["--int" "run the integration tests" (int? #t)]
   ["--opt" "run the optimization tests" (opt? #t)]
   ["--missed-opt" "run the missed optimization tests" (missed-opt? #t)]
   ["--benchmarks" "compile the typed benchmarks" (bench? #t)]
   ["--math" "compile the math library" (math? #t)]
   ["--just" path "run only this test" (single (just-one path))]
   ["--nightly" "for the nightly builds" (begin (nightly? #t) (unit? #t) (opt? #t) (missed-opt? #t) (places 1))]
   ["--all" "run all tests" (begin (unit? #t) (int? #t) (opt? #t) (missed-opt? #t) (bench? #t) (math? #t))]
   ["--excl" test "exclude tests" (excl test)]
   ["-j" num "number of places to use" 
    (let ([n (string->number num)])
      (places (and (integer? n) (> n 1) n)))]
   ["--gui" "run using the gui"
    (if (gui-available?)
        (exec go)
        (error "GUI not available"))])

  (start-workers)

  (if (and (nightly?) (eq? 'cgc (system-type 'gc)))
      (printf "Skipping Typed Racket tests.\n")
      (let ([to-run (cond [(single) (single)]
                          [else
                           (make-test-suite
                            "Typed Racket Tests"
                            (append (if (unit?)       (list unit-tests)                  '())
                                    (if (int?)        (list (int-tests (excl)))          '())
                                    (if (opt?)        (list (optimization-tests))        '())
                                    (if (missed-opt?) (list (missed-optimization-tests)) '())
                                    (if (bench?)      (list (compile-benchmarks))        '())
                                    (if (math?)       (list (compile-math))              '())))])])
        (unless (= 0 ((exec) to-run))
          (eprintf "Typed Racket Tests did not pass.\n")
          (exit 1)))))

;; nightly tests in `run.rkt` for drdr chart continuity
(module test racket/base)
