#lang typed/racket/base

(require typed/rackunit)

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
  (define b3 (ann b1 BoxTop))
  (define b4 (cast b3 (Boxof (U Integer String))))
  (check-equal? (unbox b1) 42)
  (check-equal? (unbox b4) 42)
  (check-exn #rx"expected: Integer\n *given: \"hi\""
             (λ () (set-box! b2 "hi")))
  (check-equal? (unbox b1) 42
                "if the previous test hadn't errored, this would be \"hi\" with type Integer")
  (check-exn #rx"Attempted to use a higher-order value passed as `Any`"
             (λ () (set-box! b4 "hello")))
  (check-equal? (unbox b1) 42))

(test-case "cast on mutable vectors"
  (: v1 : (Vectorof Integer))
  (define v1 (vector 42))
  (define v2 (cast v1 (Vectorof String)))
  (define v3 (ann v1 VectorTop))
  (define v4 (cast v3 (Vectorof (U Integer String))))
  (check-equal? (vector-ref v1 0) 42)
  (check-equal? (vector-ref v4 0) 42)
  (check-exn #rx"expected: Integer\n *given: \"hi\""
             (λ () (vector-set! v2 0 "hi")))
  (check-equal? (vector-ref v1 0) 42
                "if the previous test hadn't errored, this would be \"hi\" with type Integer")
  (check-exn #rx"Attempted to use a higher-order value passed as `Any`"
             (λ () (vector-set! v4 0 "hello")))
  (check-equal? (vector-ref v1 0) 42))

;; Struct definitions need to be at the module level for some reason.
(struct (X) s ([i : X]) #:mutable #:transparent)
(test-case "cast on mutable structs"
  (: s1 : (s Integer))
  (define s1 (s 42))
  (define s2 (cast s1 (s String)))
  (define s3 (ann s1 Any))
  (define s4 (cast s3 (s (U Integer String))))
  (check-equal? (s-i s1) 42)
  (check-equal? (s-i s4) 42)
  (check-exn #rx"expected: Integer\n *given: \"hi\""
             (λ () (set-s-i! s2 "hi")))
  (check-equal? (s-i s1) 42
                "if the previous test hadn't errored, this would be \"hi\" with type Integer")
  (check-exn #rx"Attempted to use a higher-order value passed as `Any`"
             (λ () (set-s-i! s4 "hello")))
  (check-equal? (s-i s1) 42))

(test-case "cast on intersections involving recursive types"
  (define-type T
    (Rec T (U String (Listof T))))
  (: f : (Listof T) -> Any)
  (define (f x)
    (if (andmap list? x)
        (cast x Any)
        #f))
  (check-equal? (f (list "a" "b" "c")) #f)
  (check-equal? (f (list (list "a") (list "b") (list "c")))
                (list (list "a") (list "b") (list "c"))))

(test-case "cast in dead code"
  (check-equal? (if #true 1 (cast 2 Integer))
                1)
  (check-equal? (if #false (cast 1 Integer) 2)
                2)
  (check-equal? (if #true 1 (list (cast 2 Integer) (cast 3 Integer)))
                1)
  (check-equal? (if #true 1 `#&,(cast 2 Integer))
                1)
  (check-equal? (if #true 1 `#(,(cast 2 Integer) ,(cast 3 Integer)))
                1)
  (check-equal? (if #true 1 `#hash([,(cast 2 Integer) . ,(cast 3 Integer)]))
                1)
  (check-equal? (if #true 1 `#s(struct ,(cast 2 Integer) ,(cast 3 Integer)))
                1)
  (check-not-false ;; check that this doesn't have an internal error
   (λ () (begin
           (error "hi")
           (cast (string->number "42") Integer))))
  )

