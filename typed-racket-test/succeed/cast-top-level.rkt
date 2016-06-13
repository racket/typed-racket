#lang racket/load

(require typed/racket/base)

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

(check-exn #rx"expected: Positive-Fixnum\n *given: 0.5"
           (λ () ((cast pos-fx-sub1 (Number -> Number)) 0.5) 4))

(check-exn #rx"expected: Positive-Fixnum\n *given: \"hello\""
           (λ () ((cast pos-fx-sub1 (String -> String)) "hello")))

(check-exn #rx"expected: Positive-Fixnum\n *given: \"hello\""
           (λ () ((cast pos-fx-sub1 (Any -> Any)) "hello")))

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
  (check-exn #rx"expected: boolean\\?\n *given: \"hello\""
             (λ () ((cast f (String -> String)) "hello")))
  (check-equal? v #t
                "if the previous test hadn't errored, this would be \"hello\" with type Boolean"))

(test-case "cast on mutable boxes"
  (: b1 : (Boxof Integer))
  (define b1 (box 42))
  (define b2 (cast b1 (Boxof String)))
  (check-equal? (unbox b1) 42)
  (check-exn #rx"expected: Integer\n *given: \"hi\""
             (λ () (set-box! b2 "hi")))
  (check-equal? (unbox b1) 42
                "if the previous test hadn't errored, this would be \"hi\" with type Integer"))

(test-case "cast on mutable vectors"
  (: v1 : (Vectorof Integer))
  (define v1 (vector 42))
  (define v2 (cast v1 (Vectorof String)))
  (check-equal? (vector-ref v1 0) 42)
  (check-exn #rx"expected: Integer\n *given: \"hi\""
             (λ () (vector-set! v2 0 "hi")))
  (check-equal? (vector-ref v1 0) 42
                "if the previous test hadn't errored, this would be \"hi\" with type Integer"))

;; Struct definitions need to be at the top level for some reason.
(struct (X) s ([i : X]) #:mutable)
(test-case "cast on mutable structs"
  (: s1 : (s Integer))
  (define s1 (s 42))
  (define s2 (cast s1 (s String)))
  (check-equal? (s-i s1) 42)
  (check-exn #rx"expected: Integer\n *given: \"hi\""
             (λ () (set-s-i! s2 "hi")))
  (check-equal? (s-i s1) 42
                "if the previous test hadn't errored, this would be \"hi\" with type Integer"))

