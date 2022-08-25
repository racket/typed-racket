#lang racket/load

(require typed/racket/base/shallow)
(require typed/rackunit)

(cast 2 Number)
(cast 2 Integer)
(cast (list 2 4) (Listof Byte))
(cast (vector 2 4) (Vectorof Byte))

(check-equal? (cast 2 Number) 2)
(check-equal? (cast 2 Integer) 2)
(check-equal? (cast (list 2 4) (Listof Byte)) (list 2 4))
(check-pred vector? (cast (vector 2 4) (Vectorof Byte)))

(check-equal? ((cast (lambda (x) 7) (String -> Number)) "seven") 7)

(: pos-fx-sub1 : Positive-Fixnum -> Nonnegative-Fixnum)
(define (pos-fx-sub1 x)
  (sub1 x))

(check-equal? ((cast pos-fx-sub1 (Number -> Number)) 5) 4)

(check-exn #rx"shape-check.*Positive-Fixnum"
           (λ () ((cast pos-fx-sub1 (Number -> Number)) 0.5) 4))

(check-exn #rx"shape-check.*Positive-Fixnum"
           (λ () (void ((cast pos-fx-sub1 (String -> String)) "hello"))))

(check-exn #rx"shape-check.*Positive-Fixnum"
           (λ () (void ((cast pos-fx-sub1 (Any -> Any)) "hello"))))

(test-case "cast on mutator functions"
  (: v : Boolean)
  (define v #f)
  (: f : Boolean -> Boolean)
  (define (f x)
    (set! v x)
    x)

  (check-equal? v #f)
  (check-equal? ((cast f (Boolean -> Boolean)) #t) #t)
  (check-equal? v #t)
  (check-exn #rx"shape-check.*Boolean"
             (λ () ((cast f (String -> String)) "hello")))
  (check-equal? v #t
                "if the previous test hadn't errored, this would be \"hello\" with type Boolean"))

(test-case "cast on mutable boxes"
  (: b1 : (Boxof Integer))
  (define b1 (box 42))
  (define b2 (cast b1 (Boxof String)))
  (check-equal? (unbox b1) 42)
  (check-not-exn
             (λ () (set-box! b2 "hi")))
  (check-not-equal? (unbox b2) 42
                "he previous test didn't error, so this value is \"hi\"")
  (check-exn #rx"shape-check"
    (lambda ()
      (unbox b1))))

(test-case "cast on mutable vectors"
  (: v1 : (Vectorof Integer))
  (define v1 (vector 42))
  (define v2 (cast v1 (Vectorof String)))
  (check-equal? (vector-ref v1 0) 42)
  (check-not-exn
             (λ () (vector-set! v2 0 "hi")))
  (check-not-equal? (vector-ref v2 0) 42
                "the previous test didn't errored, so this value is \"hi\"")
  (check-exn #rx"shape-check"
    (lambda () (vector-ref v1 0))))

;; Struct definitions need to be at the top level for some reason.
(struct (X) s ([i : X]) #:mutable)
(test-case "cast on mutable structs"
  (: s1 : (s Integer))
  (define s1 (s 42))
  (define s2 (cast s1 (s String)))
  (check-equal? (s-i s1) 42)
  (check-not-exn
             (λ () (set-s-i! s2 "hi")))
  (check-not-equal? (s-i s2) 42
                "the previous test didn't error, so this value is \"hi\"")
  (check-exn #rx"shape-check"
    (lambda () (s-i s1))))

