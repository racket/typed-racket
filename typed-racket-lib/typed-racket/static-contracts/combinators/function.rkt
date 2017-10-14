#lang racket/base

;; Static contract for ->.
;; Supports the whole range of possible options that -> does.

(require "../../utils/utils.rkt"
         "../structures.rkt" "../constraints.rkt"
         racket/list racket/match
         (contract-req)
         (for-template racket/base racket/contract/base "../../utils/simple-result-arrow.rkt")
         (for-syntax racket/base syntax/parse))

(provide ->/sc:)

(provide/cond-contract
 [function/sc (-> boolean?
                  (listof static-contract?)
                  (listof static-contract?)
                  (listof (list/c keyword? static-contract?))
                  (listof (list/c keyword? static-contract?))
                  (or/c #f static-contract?)
                  (or/c #f (listof static-contract?))
                  static-contract?)])

(struct function-combinator combinator (indices mand-kws opt-kws typed-side?)
        #:property prop:combinator-name "->/sc"
        #:methods gen:equal+hash [(define (equal-proc a b recur) (function-sc-equal? a b recur))
                                  (define (hash-proc v recur) (function-sc-hash v recur))
                                  (define (hash2-proc v recur) (function-sc-hash2 v recur))]
        #:methods gen:sc
          [(define (sc->contract v f) (function-sc->contract v f))
           (define (sc-map v f) (function-sc-map v f))
           (define (sc-traverse v f) (function-sc-map v f) (void))
           (define (sc-terminal-kind v) (function-sc-terminal-kind v))
           (define (sc->constraints v f) (function-sc-constraints v f))])

(define (split-function-args ctcs mand-args-end opt-args-end
                    mand-kw-args-end opt-kw-args-end rest-end range-end)
  (values
    (drop (take ctcs mand-args-end) 0)
    (drop (take ctcs opt-args-end) mand-args-end)
    (drop (take ctcs mand-kw-args-end) opt-args-end)
    (drop (take ctcs opt-kw-args-end) mand-kw-args-end)
    (and (> rest-end opt-kw-args-end)
         (first (drop (take ctcs rest-end) opt-kw-args-end)))
    (and range-end (drop (take ctcs range-end) rest-end))))

(define (function-sc->contract sc recur)
  (match-define (function-combinator args indices mand-kws opt-kws typed-side?) sc)

  (define-values (mand-scs opt-scs mand-kw-scs opt-kw-scs rest-sc range-scs)
    (apply split-function-args args indices))

  (define-values (mand-ctcs opt-ctcs mand-kw-ctcs opt-kw-ctcs rest-ctc range-ctcs)
    (apply split-function-args (map recur args) indices))

  (define mand-kws-stx (append-map list mand-kws mand-kw-ctcs))
  (define opt-kws-stx (append-map list opt-kws opt-kw-ctcs))
  (define rest-ctc-stx
    (if rest-ctc
      (list '#:rest rest-ctc)
      #'()))

  (define range-ctc
    (if range-ctcs
      #`(values #,@range-ctcs)
      #'any))

  (cond
   [(and (null? mand-kws) (null? opt-kws)
         (null? opt-ctcs)
         (not rest-ctc)
         ;; currently simple-result-> only handles up to arity 3
         (member (length mand-ctcs) '(0 1 2 3))
         (and range-ctcs (= 1 (length range-ctcs)))
         (for/and ([a (in-list args)]) (eq? 'flat (sc-terminal-kind a)))
         (not typed-side?))
    #`(simple-result-> #,@range-ctcs #,(length mand-ctcs))]
   [else
    #`((#,@mand-ctcs #,@mand-kws-stx)
       (#,@opt-ctcs #,@opt-kws-stx)
       #,@rest-ctc-stx
       . ->* . #,range-ctc)]))


(define (function/sc typed-side? mand-args opt-args mand-kw-args opt-kw-args rest range)
  (define mand-args-end (length mand-args))
  (define opt-args-end (+ mand-args-end (length opt-args)))
  (define mand-kw-args-end (+ opt-args-end (length mand-kw-args)))
  (define opt-kw-args-end (+ mand-kw-args-end (length opt-kw-args)))
  (define rest-end (if rest (add1 opt-kw-args-end) opt-kw-args-end))
  (define range-end (and range (+ rest-end (length range))))
  (define mand-kws (map first mand-kw-args))
  (define opt-kws (map first opt-kw-args))
  (define end-indices
    (list mand-args-end opt-args-end mand-kw-args-end opt-kw-args-end rest-end range-end))

  (function-combinator
    (append
      mand-args
      opt-args
      (map second mand-kw-args)
      (map second opt-kw-args)
      (if rest (list rest) null)
      (or range null))
    end-indices
    mand-kws
    opt-kws
    typed-side?))

(define-match-expander ->/sc:
  (syntax-parser
    [(_ mand-args opt-args mand-kw-args opt-kw-args rest range)
     #'(and (? function-combinator?)
            (app (match-lambda
                   [(function-combinator args indices mand-kws opt-kws typed-side?)
                    (define-values (mand-args* opt-args* mand-kw-args* opt-kw-args* rest* range*)
                      (apply split-function-args args indices))
                    (list
                      mand-args* opt-args*
                      (map list mand-kws mand-kw-args*)
                      (map list opt-kws opt-kw-args*)
                      rest*
                      range*)])
                 (list mand-args opt-args mand-kw-args opt-kw-args rest range)))]))

(define (function-sc-map v f)
  (match-define (function-combinator args indices mand-kws opt-kws typed-side?) v)

  (define-values (mand-args opt-args mand-kw-args opt-kw-args rest-arg range-args)
    (apply split-function-args args indices))

  (define new-args
    (append
      (map (lambda (arg) (f arg 'contravariant))
           (append mand-args opt-args mand-kw-args opt-kw-args (if rest-arg (list rest-arg) null)))
      (if range-args
          (map (lambda (arg) (f arg 'covariant))
               range-args)
          empty)))


  (function-combinator new-args indices mand-kws opt-kws typed-side?))

(define (function-sc-terminal-kind v)
  (match-define (function-combinator args indices mand-kws opt-kws typed-side?) v)
  (define-values (mand-args opt-args mand-kw-args opt-kw-args rest-arg range-args)
    (apply split-function-args args indices))
  (if (and (not rest-arg)
           (null? (append mand-kw-args mand-args opt-kw-args opt-args))
           typed-side?)
      ;; currently we only handle this trivial case
      ;; we could probably look at the actual kind of `range-args` as well
      (if (not range-args) 'flat #f)
      #f))


(define (function-sc-constraints v f)
  (match-define (function-combinator args indices mand-kws opt-kws typed-side?) v)
  (define-values (mand-args opt-args mand-kw-args opt-kw-args rest-arg range-args)
    (apply split-function-args args indices))
  (if (and (not rest-arg)
           (null? (append mand-kw-args mand-args opt-kw-args opt-args))
           typed-side?)
      ;; arity-0 functions end up being flat contracts when they're
      ;; from the typed side and the result is flat
      (if range-args
          (merge-restricts* 'chaperone (map f range-args))
          (merge-restricts* 'flat null))
      (merge-restricts* 'chaperone (map f (append (or range-args '()) args)))))

(define (function-sc-equal? a b recur)
  (match-define (function-combinator a-args a-indices a-mand-kws a-opt-kws a-typed-side?) a)
  (match-define (function-combinator b-args b-indices b-mand-kws b-opt-kws b-typed-side?) b)
  (and
    (equal? a-typed-side? b-typed-side?)
    (recur a-indices b-indices)
    (recur a-mand-kws b-mand-kws)
    (recur a-opt-kws b-opt-kws)
    (recur a-args b-args)))

(define (function-sc-hash v recur)
  (match-define (function-combinator v-args v-indices v-mand-kws v-opt-kws typed-side?) v)
  (+ (recur v-indices) (recur v-mand-kws) (recur v-opt-kws) (recur v-args)))

(define (function-sc-hash2 v recur)
  (match-define (function-combinator v-args v-indices v-mand-kws v-opt-kws typed-side?) v)
  (+ (recur v-indices) (recur v-mand-kws) (recur v-opt-kws) (recur v-args)))
