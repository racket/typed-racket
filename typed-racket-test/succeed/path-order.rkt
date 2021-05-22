#lang typed/racket

(define-type Ty1 (Promise Any))
(define-predicate ty1? Ty1)


(define (hello1 [v : Any])
  (if (and (syntax? v) (symbol? (syntax-e v)))
      (symbol->string (syntax-e v))
      (error 'hello1 "impossible")))

(define (hello2 [v : Any])
  (if (and (ty1? v) (number? (force v)))
      (add1 (force v))
      (error 'hello2 "impossible")))

(hello1 #'hihi)
(hello2 (delay 10))

(struct foo ([a : Any]))

(define (hello [v : Any])
  (if (and (foo? v) (number? (foo-a v)))
      (add1 (foo-a v))
      0))

(define (world [v : Any])
  (if (and (pair? v) (foo? (car v)) (number? (foo-a (car v))))
      (add1 (foo-a (car v)))
      555))

(define (greetings [v : Any])
  (if (and (pair? v) (or (foo? (car v)) (string? (car v))))
      (if (and (foo? (car v)) (number? (foo-a (car v))))
          (add1 (foo-a (car v)))
          42)
      555))

(struct (X) fooX ([a : X]))

(define (f0 [obj : Any]) : Number
  (cond
    [(and (foo? obj) (number? (foo-a obj))) (foo-a obj)]
    [(and (fooX? obj) (number? (fooX-a obj))) (fooX-a obj)]
    [else 42]))

(define #:forall (A) (f1 [obj : (fooX A)]) : Number
  (cond
    [(number? (fooX obj)) (fooX-a obj)]
    [else 42]))

(define (f2 [obj : foo]) : Number
  (cond
    [(number? (foo-a obj)) (foo-a obj)]
    [else 42]))
