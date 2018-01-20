#lang racket/base

(require "test-utils.rkt"
         rackunit racket/list racket/match racket/format
         syntax/srcloc syntax/location
         (types abbrev tc-result)
         (utils tc-utils)
         (rep prop-rep object-rep type-rep)
         (typecheck check-below)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)

(define x #'x)

;; Ensure that we never return a prop or object of #f.
(define (check-prop f)
  (match f
    [#f (fail-check "Result has no prop (instead of a top prop).")]
    [_ (void)]))

(define (check-object o)
  (match o
    [#f (fail-check "Result has no object (instead of an empty object).")]
    [_ (void)]))

(define (check-result result)
  (match result
    [(tc-results: (list (tc-result: _ ps os) ...) _)
     (for-each check-prop ps)
     (for-each check-object os)]
    [(tc-any-results: p)
     (check-prop p)]
    [(? Type?)
     (void)]))


(define-syntax (test-below stx)
  (syntax-parse stx
    [(_ t1:expr t2:expr (~optional (~seq #:result expected-result:expr)
                                     #:defaults [(expected-result #'t2)]))
     #`(test-case (~a 't1 " <: " 't2)
         (with-check-info* (list (make-check-location (build-source-location-list (quote-srcloc #,stx)))
                                 (make-check-expected expected-result))
           (λ ()
             (define result (check-below t1 t2))
             (with-check-info (['actual result])
               (check-result result)
               (unless (equal? expected-result result)
                 (fail-check "Check below did not return expected result."))))))]
    [(_ #:fail (~optional message:expr #:defaults [(message #'#rx"type mismatch")])
        t1:expr t2:expr
        (~optional (~seq #:result expected-result:expr)
                     #:defaults [(expected-result #'t2)]))
     #`(test-case (~a 't1 " !<: " 't2)
         (with-check-info* (list (make-check-location (build-source-location-list (quote-srcloc #,stx)))
                                 (make-check-expected expected-result))
           (λ ()
             (define result
               (parameterize ([delay-errors? #t])
                 (check-below t1 t2)))
             (with-check-info (['actual result])
               (define exn
                 (let/ec exit
                   (with-handlers [(exn:fail? exit)]
                     (report-all-errors)
                     (fail-check "Check below did not fail."))))
               (check-result result)
               (unless (equal? expected-result result)
                 (fail-check "Check below did not return expected result."))
               (check-regexp-match message (exn-message exn))))))]))


(define tests
  (test-suite "Check Below"
    (test-below -Bottom Univ)
    (test-below #:fail -Symbol -String)

    (test-below
      (ret -Bottom)
      (ret (list Univ Univ) (list -true-propset #f) (list #f -empty-obj))
      #:result (ret (list Univ Univ) (list -true-propset -ff-propset) (list -empty-obj -empty-obj)))

    (test-below
      (ret -Bottom)
      (ret (list Univ) (list #f) (list #f) Univ 'B)
      #:result (ret (list Univ) (list -ff-propset) (list -empty-obj) Univ 'B))

    ;; Bottom is not below everything if the number of values doesn't match up.
    (test-below #:fail
      (ret (list -Bottom -Bottom))
      (ret (list Univ) (list -true-propset) (list #f))
      #:result (ret (list Univ) (list -true-propset) (list -empty-obj)))

    (test-below #:fail
      (ret (list))
      (ret (list Univ) (list -true-propset) (list #f))
      #:result (ret (list Univ) (list -true-propset) (list -empty-obj)))

    (test-below
      (ret (list -Symbol) (list -tt-propset) (list -empty-obj))
      (ret (list Univ) (list #f) (list #f))
      #:result (ret (list Univ) (list -tt-propset) (list -empty-obj)))

    (test-below
      (ret (list -Symbol) (list -true-propset) (list -empty-obj))
      (ret (list Univ) (list -tt-propset) (list -empty-obj)))

    (test-below #:fail
      (ret (list -Symbol) (list -tt-propset) (list -empty-obj))
      (ret (list Univ) (list -true-propset) (list #f))
      #:result (ret (list Univ) (list -true-propset) (list -empty-obj)))

    (test-below #:fail #rx"no object"
      (ret (list -Symbol) (list -tt-propset) (list -empty-obj))
      (ret (list Univ) (list -tt-propset) (list (make-Path empty #'x))))

    (test-below #:fail #rx"no object"
      (ret (list -Symbol) (list -tt-propset) (list -empty-obj))
      (ret (list Univ) (list -true-propset) (list (make-Path empty #'x))))

    (test-below (ret -Bottom) (-tc-any-results #f) #:result (-tc-any-results -ff))
    (test-below (ret Univ) (-tc-any-results -tt) #:result (-tc-any-results -tt))
    (test-below (-tc-any-results -ff) (-tc-any-results #f) #:result (-tc-any-results -ff))
    (test-below
      (ret (list -Symbol -String) (list -true-propset -ff-propset))
      (-tc-any-results #f)
      #:result (-tc-any-results -ff))
    (test-below (ret -Symbol -ff-propset) (-tc-any-results #f) #:result (-tc-any-results -ff))

    (test-below (ret -Symbol -true-propset -empty-obj) (-tc-any-results #f)
      #:result (-tc-any-results -tt))
    (test-below (ret (list -Symbol -String)) (-tc-any-results #f)
      #:result (-tc-any-results -tt))
    (test-below
      (ret (list -Symbol -String) (list -true-propset -false-propset) (list -empty-obj -empty-obj))
      (-tc-any-results #f)
      #:result (-tc-any-results -tt))


    (test-below #:fail
      (ret -Symbol)
      (ret (list -Symbol -Symbol) (list -tt-propset #f) (list #f -empty-obj))
      #:result (ret (list -Symbol -Symbol) (list -tt-propset -tt-propset) (list -empty-obj -empty-obj)))

    (test-below #:fail
      (-tc-any-results -tt)
      (ret -Symbol))


    (test-below #:fail
      (ret -Symbol -true-propset -empty-obj)
      (ret -Symbol -true-propset -empty-obj Univ 'B))

    (test-below #:fail
      (ret -Symbol -true-propset -empty-obj Univ 'B)
      (ret -Symbol -true-propset -empty-obj))

    (test-below #:fail
      (ret -Symbol)
      (ret -Symbol #f -empty-obj Univ 'B)
      #:result (ret -Symbol -tt-propset -empty-obj Univ 'B))

    (test-below #:fail
      (-tc-any-results -tt)
      (ret -Symbol #f -empty-obj Univ 'B)
      #:result (ret (list -Symbol) (list -tt-propset) (list -empty-obj) Univ 'B))

    (test-below #:fail
      (ret -Symbol -tt-propset -empty-obj Univ 'B)
      (ret (list -Symbol -Symbol) (list -tt-propset -tt-propset)  (list -empty-obj -empty-obj) Univ 'B))

    (test-below (ret -Symbol -true-propset -empty-obj Univ 'B)
                (-tc-any-results #f)
                #:result (-tc-any-results -tt))

    (test-below
      (ret -Symbol)
      (ret -Symbol #f -empty-obj)
      #:result (ret -Symbol -tt-propset -empty-obj))

    (test-below
      (ret -Symbol -true-propset)
      (ret -Symbol #f -empty-obj)
      #:result (ret -Symbol -true-propset -empty-obj))

    (test-below #:fail
      (ret -Symbol -true-propset)
      (ret (list Univ -Symbol) (list #f -tt-propset))
      #:result (ret (list Univ -Symbol) (list -tt-propset -tt-propset)))


    (test-below
      (ret (list Univ) (list -true-propset) (list -empty-obj))
      (ret Univ #f)
      #:result (ret (list Univ) (list -true-propset) (list -empty-obj)))

    ;; Enable these once check-below is fixed
    ;; Currently does not fail
    #;
    (test-below #:fail
      (ret (list Univ) (list -tt-propset) (list -empty-obj) Univ 'B)
      (ret (list Univ) (list -false-propset) (list #f) Univ 'B)
      #:result (ret (list Univ) (list -false-propset) (list -empty-obj) Univ 'B))

    ;; Currently does not fail
    #;
    (test-below #:fail
      (ret (list Univ) (list -tt-propset) (list -empty-obj))
      (ret (list Univ) (list -false-propset) (list #f) Univ 'B)
      #:result (ret (list Univ) (list -false-propset) (list -empty-obj) Univ 'B))

    ;; Currently does not fail
    #;
    (test-below #:fail
      (ret (list Univ Univ) (list -tt-propset -tt-propset) (list -empty-obj -empty-obj))
      (ret (list Univ Univ) (list -false-propset -false-propset) (list #f #f))
      #:result (ret (list Univ Univ) (list -false-propset -false-propset) (list -empty-obj -empty-obj)))

  ))
