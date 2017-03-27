#lang racket/base

#|
This file is for utilities that are of general interest,
at least theoretically.
|#

(require (for-syntax racket/base racket/string)
         racket/require-syntax racket/provide-syntax
         racket/match
         racket/list
         syntax/parse/define
         racket/struct-info "timing.rkt")

(provide
 ;; optimization
 optimize?
 ;; parameter to toggle linear integer reasoning
 with-linear-integer-arithmetic?
 ;; timing
 start-timing do-time
 ;; provide macros
 rep utils typecheck infer env private types static-contracts
 ;; misc
 list-extend
 ends-with?
 filter-multiple
 syntax-length
 in-pair
 in-sequence-forever
 match*/no-order
 bind
 genid
 gen-pretty-id
 local-tr-identifier?
 mark-id-as-normalized
 normalized-id?)

(define optimize? (make-parameter #t))
(define with-linear-integer-arithmetic? (make-parameter #f))
(define-for-syntax enable-contracts? (and (getenv "PLT_TR_CONTRACTS") #t))

(define-syntax do-contract-req
  (if enable-contracts?
      (lambda (stx) (datum->syntax stx '(begin
                                         (require racket/contract/base)
                                         (require racket/contract/region))))
      (syntax-rules () [(_) (begin)])))
(do-contract-req)

;; fancy require syntax
(define-syntax (define-requirer stx)
  (syntax-parse stx
    [(_ nm:id nm-out:id)
     #`(...
        (begin
          (define-require-syntax (nm stx)
            (syntax-parse stx
              [(form id:identifier ...)
               (with-syntax ([(id* ...)
                              (for/list ([id (syntax->list #'(id ...))])
                                (datum->syntax
                                 id
                                 `(lib
                                   ,(datum->syntax
                                     #f
                                     (string-join
                                      (list "typed-racket"
                                            (symbol->string (syntax-e #'nm))
                                            (string-append (symbol->string (syntax-e id)) ".rkt"))
                                      "/")
                                     id id))
                                 id id))])
                 (syntax-property (syntax/loc stx (combine-in id* ...))
                                  'disappeared-use
                                  #'form))]))
          (define-provide-syntax (nm-out stx)
            (syntax-parse stx
              [(_ id:identifier ...)
               (with-syntax ([(id* ...)
                              (for/list ([id (syntax->list #'(id ...))])
                                (datum->syntax
                                 id
                                 `(lib
                                   ,(datum->syntax
                                     #f
                                     (string-join
                                      (list "typed-racket"
                                            (symbol->string (syntax-e #'nm))
                                            (string-append (symbol->string (syntax-e id)) ".rkt"))
                                      "/")
                                     id id))))])
                 (syntax/loc stx (combine-out (all-from-out id*) ...)))]))
          (provide nm nm-out)))]))


(define-requirer rep rep-out)
(define-requirer infer infer-out)
(define-requirer typecheck typecheck-out)
(define-requirer utils utils-out)
(define-requirer env env-out)
(define-requirer private private-out)
(define-requirer types types-out)
(define-requirer logic logic-out)
(define-requirer optimizer optimizer-out)
(define-requirer base-env base-env-out)
(define-requirer static-contracts static-contracts-out)

;; turn contracts on and off - off by default for performance.
(provide (for-syntax enable-contracts?)
         provide/cond-contract
         with-cond-contract
         define-struct/cond-contract
         define/cond-contract
         contract-req
         define/provide
         define/cond-contract/provide
         define-for-cond-contract
         provide-for-cond-contract
         require-for-cond-contract
         begin-for-cond-contract)



(define-require-syntax contract-req
  (if enable-contracts?
      (lambda (stx) (datum->syntax stx 'racket/contract))
      (syntax-rules ()
        [(_) (combine-in)])))

(define-syntax define-for-cond-contract
  (if enable-contracts?
      (make-rename-transformer #'define)
      (syntax-parser
        [(_ args:expr body:expr) #'(begin)])))

(define-syntax provide-for-cond-contract
  (if enable-contracts?
      (make-rename-transformer #'provide)
      (syntax-parser
        [(_ provide-spec:expr ...) #'(begin)])))

(define-syntax require-for-cond-contract
  (if enable-contracts?
      (make-rename-transformer #'require)
      (syntax-parser
        [(_ require-spec:expr ...) #'(begin)])))

(define-syntax begin-for-cond-contract
  (if enable-contracts?
      (make-rename-transformer #'begin)
      (syntax-parser
        [(_ e:expr ...) #'(begin)])))


(define-syntax (define/provide stx)
  (syntax-parse stx
    [(_ name:id . body)
     (syntax/loc stx
       (begin (define name . body)
              (provide name)))]
    [(_ (name:id . args) . body)
     (syntax/loc stx
       (begin (define (name . args) . body)
              (provide name)))]))

(define-simple-macro (define/cond-contract/provide (name:id . args) c . body)
  (begin (define (name . args) . body)
         (provide/cond-contract [name c])))

;; these are versions of the contract forms conditionalized by `enable-contracts?'
(define-syntax provide/cond-contract
  (if enable-contracts?
      (make-rename-transformer #'provide/contract)
      (lambda (stx)
        (define-syntax-class clause
          #:attributes (i)
          (pattern [(~datum struct) (~or nm:id (nm:id super:id)) (flds ...)]
                   #:with i #'(struct-out nm))
          (pattern [(~datum rename) out:id in:id cnt:expr]
                   #:with i #'(rename-out [out in]))
          (pattern [i:id cnt:expr]))
        (syntax-parse stx
          [(_ c:clause ...)
           #'(provide c.i ...)]))))

(define-syntax with-cond-contract
  (if enable-contracts?
      (make-rename-transformer #'with-contract)
      (lambda (stx)
        (syntax-parse stx
          [(_ name (~or #:results #:result) spec . body)
           #'(let () . body)]
          [(_ name specs . body)
           #'(begin . body)]))))

(define-syntax define/cond-contract
  (if enable-contracts?
      (make-rename-transformer #'define/contract)
      (lambda (stx)
        (syntax-parse stx
          [(_ head cnt . body)
           (syntax/loc stx (define head . body))]))))

(define-syntax define-struct/cond-contract
  (if enable-contracts?
      (make-rename-transformer #'define-struct/contract)
      (syntax-rules ()
        [(_ hd ([i c] ...) . opts)
         (define-struct hd (i ...) . opts)])))


(provide make-struct-info-self-ctor)
;Copied from racket/private/define-struct
;FIXME when multiple bindings are supported
(define (self-ctor-transformer orig stx)
  (define (transfer-srcloc orig stx)
    (datum->syntax orig (syntax-e orig) stx orig))
  (syntax-case stx ()
    [(self arg ...) (datum->syntax stx
                                   (cons (syntax-property (transfer-srcloc orig #'self)
                                                          'constructor-for
                                                          (syntax-local-introduce #'self))
                                         (syntax-e (syntax (arg ...))))
                                   stx
                                   stx)]
    [_ (transfer-srcloc orig stx)]))


(define make-struct-info-self-ctor
 (let ()
  (struct struct-info-self-ctor (id info)
          #:property prop:procedure
                     (lambda (ins stx)
                      (self-ctor-transformer (struct-info-self-ctor-id ins) stx))
          #:property prop:struct-info (λ (x) (extract-struct-info (struct-info-self-ctor-info x))))
  struct-info-self-ctor))

;; Listof[A] Listof[B] B -> Listof[B]
;; pads out t to be as long as s
(define (list-extend s t extra)
  (append t (build-list (max 0 (- (length s) (length t))) (lambda _ extra))))

;; does l1 end with l2?
;; e.g. (list 1 2 3) ends with (list 2 3)
(define (ends-with? l1 l2)
  (define len1 (length l1))
  (define len2 (length l2))
  (and (<= len2 len1)
       (equal? l2 (drop l1 (- len1 len2)))))

(define (filter-multiple l . fs)
  (apply values
         (map (lambda (f) (filter f l)) fs)))

(define (syntax-length stx)
  (let ((list (syntax->list stx)))
    (and list (length list))))

(define (in-sequence-forever seq val)
  (make-do-sequence
   (λ ()
     (let-values ([(more? gen) (sequence-generate seq)])
       (values (λ (e) (if (more?) (gen) val))
               (λ (_) #t)
               #t
               (λ (_) #t)
               (λ _ #t)
               (λ _ #t))))))

(define-syntax (match*/no-order stx)
  (define (parse-clauses clauses)
    (syntax-parse clauses
      [() #'()]
      [([(lpat rpat) #:no-order . body]
        . rst)
       #`([(lpat rpat) . body]
          [(rpat lpat) . body]
          . #,(parse-clauses #'rst))]
      [((~and cl [(lpat rpat) . body])
        . rst)
       #`(#,(syntax/loc #'cl [(lpat rpat) . body])
          . #,(parse-clauses #'rst))]))
  (syntax-parse stx
    [(_ (val1:expr val2:expr)
        . clauses)
     #`(match* (val1 val2)
         . #,(parse-clauses #'clauses))]))


(define-match-expander bind
  (syntax-parser
    [(_ x:id val:expr)
     #'(app (λ (_) val) x)]))

(define-syntax (assert stx)
    (syntax-case stx ()
      [(_ expr)
       #`(unless expr #,(quasisyntax/loc stx (error 'assert "failed!")))]))


(define-syntax-rule (in-pair p)
  (in-parallel (in-value (car p)) (in-value (cdr p))))


(module local-ids racket
  (provide local-tr-identifier?
           genid
           gen-pretty-id
           mark-id-as-normalized
           normalized-id?)
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
  ;; generates fresh identifiers for use while typechecking
  (define (genid [sym (gensym 'local)])
    (mark-id-as-normalized (datum->syntax #'loc sym)))
  (define letters '("x" "y" "z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
                        "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"  "w"))
  ;; this is just a silly helper function that gives us a letter from
  ;; the latin alphabet in a cyclic manner
  (define next-letter
    (let ([i 0])
      (λ ()
        (define letter (string->uninterned-symbol (list-ref letters i)))
        (set! i (modulo (add1 i) (length letters)))
        letter)))
  ;; generates a fresh identifier w/ a "pretty" printable representation
  (define (gen-pretty-id [sym (next-letter)])
    (mark-id-as-normalized (datum->syntax #'loc sym)))
  ;; allows us to recognize and distinguish gensym'd identifiers
  ;; from ones that came from the program we're typechecking
  (define (local-tr-identifier? id)
    (and (identifier? id)
         (eq? (syntax-source-module dummy-id)
              (syntax-source-module id)))))

(require 'local-ids)
