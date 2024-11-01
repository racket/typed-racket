#lang racket/base

(require (for-syntax racket/base
                     racket/format
                     syntax/parse)
         racket/base
         racket/port
         racket/promise
         rackunit
         "test-utils.rkt")

(provide tests)
(gen-test-main)

(define-namespace-anchor anchor)

(define base-ns
  (delay
    (define ns (namespace-anchor->empty-namespace anchor))
    (parameterize ([current-namespace ns])
      (namespace-require 'typed/racket/base))
    ns))

;; get-ns: boolean? -> namespace?
;; Returns a namespace with the bindings of typed/racket/base.
;; If the argument is true, then it is a new namespace. This is slower but allows for tests that need
;; to mutate the namespace to not clash with each other.
(define (get-ns fresh)
  (cond
    [fresh
     (define ns (variable-reference->empty-namespace (eval '(#%variable-reference) (force base-ns))))
     (parameterize ([current-namespace ns])
       (namespace-require 'typed/racket/base)
       ns)]
    [else (force base-ns)]))

(begin-for-syntax
  (define-splicing-syntax-class fresh-kw
    (pattern (~seq) #:attr fresh #'#f)
    (pattern #:fresh #:attr fresh #'#t)))

(define-syntax (test-form-exn stx)
  (syntax-parse stx
    [(_ f:fresh-kw regexp:expr form:expr)
     (quasisyntax/loc stx
       (test-case #,(~a (syntax->datum #'form))
         (check-exn
           regexp
           (lambda ()
             (eval `(#%top-interaction .
                     ,(syntax->datum #'form)) (get-ns f.fresh))))))]))

(define-syntax (test-form-not-exn stx)
  (syntax-parse stx
    [(_ f:fresh-kw form:expr)
     (quasisyntax/loc stx
       (test-case #,(~a (syntax->datum #'form))
         (check-not-exn
           (lambda ()
             (eval `(#%top-interaction .
                     ,(syntax->datum #'form)) (get-ns f.fresh))))))]))

(define-syntax (test-form stx)
  (syntax-parse stx
    [(_ f:fresh-kw (~seq regexp:expr form:expr) ...)
     (quasisyntax/loc stx
       (test-case #,(~a (syntax->datum #'(form ...)))
         (define ns (get-ns f.fresh))
         (check-regexp-match
           regexp
           (with-output-to-string
             (lambda ()
               (eval `(#%top-interaction .
                       ,(syntax->datum #'form)) ns)))) ...))]))

;; Add 'only at the toplevel tests'
(define tests
  (test-suite "Interactive tests"

    (test-form #:fresh
      #rx"" (module test racket)
      #rx"" (define module displayln)
      #rx"racket" (module 'racket))

    (test-form (regexp-quote "String")
      "foo")
    (test-form (regexp-quote "String")
      (begin "foo"))
    (test-form (regexp-quote "String")
      (begin "foo" "bar"))
    (test-form #rx"^$"
      (begin))
    (test-form #rx"^$"
      (define x "foo"))
    (test-form #rx"^$"
      (begin (: x String)
             (define x "foo")))
    (test-form #rx"^$"
               (struct foo ()))


    (test-form #rx"^$"
               (begin
                 (: prop (Struct-Property (-> Self Number)))
                 (: pred (-> Any Boolean : (Has-Struct-Property prop)))
                 (: acc (-> (Has-Struct-Property prop) (Some (X) (-> X Number) : #:+ X)))
                 (define-values (prop pred acc) (make-struct-type-property 'prop))))

    ;; Make sure that optimized expressions work
    (test-form #rx"Flonum"
      (+ 1.0 2.0))

    ;; PR 14487
    (test-form-not-exn
      (begin
       (require/typed racket/base
                      [#:opaque Evt evt?]
                      [alarm-evt (Real -> Evt)]
                      [sync (Evt -> Any)])
       (void evt?)))

    ;; PR 14380
    (test-form-not-exn (begin - (void)))

    ;; bug that delayed 6.3
    (test-form-exn #rx"Any"
     (let ((x : Any 0))
       (define (f) (set! x #t))
       (when (number? x)
         (add1 x))))

    (test-form-not-exn
     (let ((x 0))
       (set! x 1)))

    ;; test message for undefined id
    (test-form-exn #rx"either undefined or missing a type annotation"
      (a-name-that-isnt-bound))

    ;; Make sure unannotated definitions with the wrong number of values
    ;; don't produce an internal error
    (test-form-exn #rx"Expression should produce 1 values"
      (define zzzzz (values 1 2)))

    (test-form #rx"1"
      (:type 1))
    (test-form (regexp-quote "(U Positive-Byte Zero)")
      (:type Byte))
    (test-form (regexp-quote "(U 0 1 Byte-Larger-Than-One")
      (:type #:verbose Byte))
    (test-form-exn #rx":type.*applied to arguments"
      :type)
    (test-form-exn #rx":type.*only valid at the top-level"
      (list (:type)))
    (test-form-exn #rx"exactly one argument"
      (:type))
    (test-form-exn #rx"exactly one argument"
      (:type 1 2))
    (test-form-exn #rx"exactly one argument"
      (:type #:verbose))

    (test-form #rx"Positive-Index"
      (:print-type (+ 1 1)))

    (test-form #rx"\\*"
      (:kind Number))

    (test-form #rx"\\*"
               (:kind (Pairof Number Number)))

    (test-form (regexp-quote "(-o * ... *)") (:kind U))
    (test-form (regexp-quote "(-o * ... *)") (:kind Union))
    (test-form (regexp-quote "(-o * ... *)") (:kind Intersection))
    (test-form (regexp-quote "(-o * ... *)") (:kind ∩))
    (test-form (regexp-quote "(-> * ... *)") (:kind Vector))
    (test-form (regexp-quote "(-> * ... *)") (:kind Immutable-Vector))
    (test-form (regexp-quote "(-> * * *)") (:kind Pairof))
    (test-form (regexp-quote "(-> * *)") (:kind Listof))
    (test-form (regexp-quote "(-> * * *)") (:kind MPairof))
    (test-form (regexp-quote "(-> * *)") (:kind Listof))
    (test-form (regexp-quote "(-> * *)") (:kind Immutable-Vectorof))
    (test-form (regexp-quote "(-> * *)") (:kind Mutable-Vectorof))
    (test-form (regexp-quote "(-o * *)") (:kind Option))
    (test-form (regexp-quote "(-> * * *)") (:kind Immutable-HashTable))
    (test-form (regexp-quote "(-> * * *)") (:kind Mutable-HashTable))
    (test-form (regexp-quote "(-> * *)") (:kind Promise))
    (test-form (regexp-quote "(-> * *)") (:kind Boxof))
    (test-form (regexp-quote "(-> * *)") (:kind Channelof))
    (test-form (regexp-quote "(-> * *)") (:kind Async-Channelof))
    (test-form (regexp-quote "(-> * *)") (:kind Setof))
    (test-form (regexp-quote "(-> * *)") (:kind Evtof))
    (test-form (regexp-quote "(-> * *)") (:kind Futureof))
    (test-form (regexp-quote "(-> * *)") (:kind MListof))
    (test-form (regexp-quote "(-> * *)") (:kind Thread-Cellof))
    (test-form (regexp-quote "(-> * *)") (:kind Weak-Boxof))
    (test-form (regexp-quote "(-> * *)") (:kind Vectorof))
    (test-form-not-exn (define-type (Bar a) a))
    (test-form (regexp-quote "(-o * *)")
               (:kind Bar))


    (test-form #rx"more precisely: Positive-Byte"
               10)

    (test-form #rx"\\(MPairof String Null\\)\n$"
               (begin (: a-pair (MPairof String Null))
                      (define a-pair (mcons "123456" '()))
                      a-pair))
    (test-form #rx"\\(Immutable-Vectorof Number\\)\n$"
               (ann (vector-immutable 10 20 30) (Immutable-Vectorof Number)))
    (test-form #rx"\\(Mutable-Vector Integer Integer Integer\\)\n$"
               (vector 10 20 30))
    (test-form #rx"\\(Mutable-Vectorof Number\\)\n$"
               (ann (vector 10 20 30) (Mutable-Vectorof Number)))
    (test-form (regexp-quote "(values One One)")
      (:print-type (values 1 1)))
    (test-form-exn #rx":print-type.*applied to arguments"
      :print-type)
    (test-form-exn #rx":print-type.*only valid at the top-level"
      (list (:print-type)))
    (test-form-exn #rx"exactly one argument"
      (:print-type))
    (test-form-exn #rx"exactly one argument"
      (:print-type 1 2))

    (test-form (regexp-quote "(-> 4 Zero Zero)")
      (:query-type/args * 4 0))
    (test-form-exn #rx":query-type/args.*applied to arguments"
      :query-type/args)
    (test-form-exn #rx":query-type/args.*only valid at the top-level"
      (list (:query-type/args)))
    (test-form-exn #rx"at least one argument"
      (:query-type/args))

    (test-form (regexp-quote "(-> One)")
      (:query-type/result * 1))
    (test-form #rx"not in the given function's range.\n"
      (:query-type/result + String))
    (test-form-exn #rx":query-type/result.*applied to arguments"
      :query-type/result)
    (test-form-exn #rx":query-type/result.*only valid at the top-level"
      (list (:query-type/result)))
    (test-form-exn #rx"exactly two arguments"
      (:query-type/result))
    (test-form-exn #rx"exactly two arguments"
      (:query-type/result 1 2 3))
    (test-form #rx"not in the given function's range"
      (:query-type/result syntax-local-expand-expression Boolean))

    (test-form-exn (regexp (regexp-quote "expected: (-> john-doe Output-Port (U Boolean One Zero) AnyValues)"))
      (struct john-doe () #:property prop:custom-write (lambda ([a : Integer] [b : Integer] [c : Integer]) : Void
                                                         (void))))
    (test-form #rx"^$"
      (struct animal ([a : Number] [b : (-> Number Number)]) #:property prop:procedure (struct-field-index b)))

    (test-form #rx"^$"
      (struct cat animal ([c : Number])))

    (test-form #rx"^$"
      (struct c-cat cat ([d : (-> Symbol String)]) #:property prop:procedure (struct-field-index d)))
    (test-form #rx"String"
      ((c-cat 2 add1 42 symbol->string) 'aabbcc))
    ;; TR GH issues 541 and 532.
    (test-form
     #rx"^$"
     (module mod-a typed/racket
       (provide (all-defined-out))
       (struct Foo ())
       (define-type (Bar T) (∩ T Foo))))
    (test-form #rx"^$"
      (require 'mod-a))
    (test-form (regexp-quote "Nothing")
               (:type (Bar Symbol)))))
