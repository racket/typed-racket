#lang typed/racket


;; This checks that restrict can successfully update
;; by unfolding recursive definitions and structurally updating
;; down car/cdr paths
(define: (number-of-macro-definitions [expr : Sexp]) : Number
  (match expr
    [`(define-syntaxes (,s . ,z ). ,_ ) ;`(define-syntaxes (,s ...) ,_ ...)
     (if (and (list?  expr) (list? z))
         (length (cons s z));;s -> cadr expr
         (error "corrupted file"))]
    [_ 0]))
(define: (num-of-define-syntax [exprs : (Listof Sexp)]) : Number
  (foldl (lambda: ([t : Sexp] [r : Number]) (+ (number-of-macro-definitions t) r)) 0 exprs))

(struct snafu ([x : Number]))

#;(define (foo [f : snafu]) : Integer ;; doesn't (i.e. shouldn't) typecheck
  (if (exact-integer? (snafu-x f))
      (snafu-x f)
      42))

#;(define (goo [f : snafu]) : Integer ;; doesn't (i.e. shouldn't) typecheck
  (let ([n (snafu-x f)])
    (if (exact-integer? n)
        (snafu-x f)
        42)))

(define (bar [f : snafu]) : Integer
  (let ([n (snafu-x f)])
    (if (exact-integer? n)
        n
        42)))

(define (baz [f : snafu]) : Integer
  (let ([n (snafu-x f)])
    (if (exact-integer? (snafu-x f))
        n
        42)))

