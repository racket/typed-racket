#lang typed/racket/base/shallow

;; Test functions args, positional optional keyword rest

;; -----------------------------------------------------------------------------
;; --- positional

(: f0 (-> Natural))
(define (f0)
  4)

(lambda () 0)


(: f1 (-> Natural Natural))
(define (f1 x)
  (+ x 1))

(lambda ((x : Natural))
  (+ x 1))


(: f2 (-> (Listof Natural) (Vectorof Natural) Natural))
(define (f2 a b)
  (+ (length a) (vector-length b)))

(lambda ((a : (Listof Natural)) (b : (Vectorof Natural)))
  (+ (length a) (vector-length b)))

;; -----------------------------------------------------------------------------
;; --- optional

(: f3 (->* [(Listof Natural)] [(U #f Symbol)] Natural))
(define (f3 a [b #f])
  (+ (length a) (if b (string-length (symbol->string b)) 1)))

(lambda ((a : (Listof Natural)) (b : (U #f Symbol) #f))
  (+ (length a) (if b (string-length (symbol->string b)) 1)))

(: f4 (->* [] [Integer (U #f Symbol)] Natural))
(define (f4 [a -1] [b #f])
  (+ (abs a) (if b (string-length (symbol->string b)) 1)))

(lambda ([a : Integer -1] [b : (U #f Symbol) #f])
  (+ (abs a) (if b (string-length (symbol->string b)) 1)))

;; -----------------------------------------------------------------------------
;; --- keyword

(: f5 (-> #:a Symbol #:b (Listof Symbol) String))
(define (f5 #:a a #:b b)
  (if (null? b) "hello" "world"))

(lambda (#:a (a : Symbol) #:b (b : (Listof Symbol)))
  (if (null? b) "hello" "world"))

(: f6 (->* [] [#:a Symbol #:b (Listof Symbol)] String))
(define (f6 #:a [a 'a] #:b [b '(b b b)])
  (if (= 3 (length b)) "one" "two"))

(lambda (#:a [a : Symbol 'a] #:b [b : (Listof Symbol) '(b b b)])
  (if (= 3 (length b)) "one" "two"))

;; -----------------------------------------------------------------------------
;; --- rest

(: f7 (->* [] [] #:rest String String))
(define (f7 . xs)
  (if (null? xs)
    "asdf"
    (string-append (car xs) (cadr xs))))

(f7)
(f7 "a" "b")

(ann
  (lambda xs
    (string-append (car xs) (cadr xs)))
  (->* [] [] #:rest String String))

;; -----------------------------------------------------------------------------
;; --- misc

(: choose-randomly (->* [ (Listof Symbol) Natural] [ #:random (U #f String)] String))
(define (choose-randomly probabilities speed #:random (q #false))
  (cond
   [(string? q)
    "string"]
   [(not q)
    "false"]
   [else
    "oh no something else"]))

(choose-randomly '() 0 #:random "yes")
(choose-randomly '() 0 #:random #false)
(choose-randomly '() 0) ; this will error if keyword-arg functions are protected

(define-type State Natural)
(define-type Payoff Natural)
(define-type Transition* (Listof Symbol))

(struct automaton ({current : State}
                   {original : State}
                   {payoff : Payoff}
                   {table : Transition*}) #:transparent)

(: make-random-automaton (-> Natural automaton))
(define (make-random-automaton n)
  (automaton n n n '()))

;; -----------------------------------------------------------------------------
;; --- kcfa

(struct Exp ())

(struct Call Exp ([a : Any] [b : Any] [c : Any]))

(define (new-label)
  'aaa)

(: make-call (-> Exp Exp * Exp))
(define (make-call fun . args)
  (Call (new-label) fun args))

;; -----------------------------------------------------------------------------
;; --- polydots

;; Do NOT want to see "error shape-check expected Nothing"
((plambda: (x ...) [xs : x ... x] xs) 3 4 5)

;; -----------------------------------------------------------------------------
;; --- poly

(: shuffle-vector
   (All (X) (-> (Vectorof X) (Vectorof X) (cons (Vectorof X) (Vectorof X)))))
(define (shuffle-vector b a)
  ;; copy b into a
  (for ([x (in-vector b)][i (in-naturals)])
    (vector-set! a i x))
  ;; now shuffle a 
  (for ([x (in-vector b)] [i (in-naturals)])
    (define j (random (add1 i)))
    (unless (= j i) (vector-set! a i (vector-ref a j)))
    (vector-set! a j x))
  (cons a b))

(shuffle-vector (vector 1 2 3) (vector 4 5 6))

