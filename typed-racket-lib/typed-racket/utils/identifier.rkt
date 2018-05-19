#lang racket/base

(require (for-syntax racket/base))

(provide local-tr-identifier?
         genid
         gen-pretty-sym
         gen-pretty-id
         symbol->fresh-pretty-normal-id
         gen-existential-id
         mark-id-as-normalized
         normalized-id?
         existential-id?
         with-printable-names)
;; we use this syntax location to recognized gensymed identifiers
(define-for-syntax loc #'x)
(define dummy-id (datum->syntax #'loc (gensym 'x)))
;; tools for marking identifiers as normalized and recognizing normalized
;; identifiers (we normalize ids so free-identifier=? ids are represented
;; with the same syntax object and are thus equal?)
(define-values (mark-id-as-normalized
                normalized-id?)
  (let ([normalized-identifier-sym (gensym 'normal-id)])
    (values (λ (id) (syntax-property id normalized-identifier-sym #t))
            (λ (id) (syntax-property id normalized-identifier-sym)))))
(define-values (mark-id-as-existential
                existential-id?)
  (let ([existential-identifier-sym (gensym 'existential-id)])
    (values (λ (id) (syntax-property id existential-identifier-sym #t))
            (λ (id) (syntax-property id existential-identifier-sym)))))
;; generates fresh identifiers for use while typechecking
(define (genid [sym (gensym 'local)])
  (mark-id-as-normalized (datum->syntax #'loc sym)))
(define letters (vector-immutable "x" "y" "z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
                                  "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"  "w"))
(define subscripts (vector-immutable "₀" "₁" "₂" "₃" "₄" "₅" "₆" "₇" "₈" "₉"))
;; this is just a silly helper function that gives us a letter from
;; the latin alphabet in a cyclic manner
(define gen-pretty-sym
  (let ([i 0])
    (λ ()
      (define letter (string->uninterned-symbol (vector-ref letters i)))
      (set! i (modulo (add1 i) (vector-length letters)))
      letter)))
;; generates a fresh identifier w/ a "pretty" printable representation
(define (gen-pretty-id [sym (gen-pretty-sym)])
  (mark-id-as-normalized (datum->syntax #'loc sym)))
;; generates a fresh identifier w/ a "pretty" printable representation
;; (i.e. looks like the given sym)
(define (symbol->fresh-pretty-normal-id sym)
  (mark-id-as-normalized (datum->syntax #'loc (string->uninterned-symbol (symbol->string sym)))))
(define (gen-existential-id [sym (gen-pretty-sym)])
  (mark-id-as-existential (genid sym)))
;; allows us to recognize and distinguish gensym'd identifiers
;; from ones that came from the program we're typechecking
(define (local-tr-identifier? id)
  (and (identifier? id)
       (eq? (syntax-source-module dummy-id)
            (syntax-source-module id))))
(define (nat->id n)
  (define-values (subscript letter-idx)
    (quotient/remainder n (vector-length letters)))
  (define letter (vector-ref letters letter-idx))
  (let loop ([sub ""]
             [left subscript])
    (define next-digit (vector-ref subscripts (remainder left 10)))
    (cond
      [(< left 10)
       (mark-id-as-normalized
        (datum->syntax
         #'loc (string->uninterned-symbol (string-append letter next-digit sub))))]
      [else
       (loop (string-append next-digit sub)
             (quotient left 10))])))

(define pretty-fresh-name-counter (make-parameter 0))
(define-syntax-rule (with-printable-names count-expr ids . body)
  (let ([offset (pretty-fresh-name-counter)]
        [count count-expr])
    (parameterize ([pretty-fresh-name-counter (+ count offset)])
      (let ([ids (for/list ([n (in-range offset (+ offset count))])
                   (nat->id n))])
        . body))))