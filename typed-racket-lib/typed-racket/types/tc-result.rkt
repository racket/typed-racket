#lang racket/base

(require "../utils/utils.rkt"
         (rep core-rep type-rep prop-rep values-rep)
         (utils tc-utils)
         (types base-abbrev)
         racket/match
         (prefix-in c: (contract-req)))

;; this structure represents the result of typechecking an expression
;; fields are #f only when the direct result of parsing or annotations
(define-struct/cond-contract tc-result
  ([t Type?] [pset (c:or/c PropSet? #f)] [o (c:or/c OptObject? #f)])
  #:transparent)
(define-struct/cond-contract tc-results
  ([ts (c:listof tc-result?)] [drest (c:or/c (c:cons/c Type? symbol?) #f)])
  #:transparent)
(define-struct/cond-contract tc-any-results ([f (c:or/c Prop? #f)]) #:transparent)

(define (tc-results/c v)
  (or (tc-results? v)
      (tc-any-results? v)))

(define (tc-results1/c v)
  (and (tc-results? v)
       (= (length (tc-results-ts v)) 1)))

;; Contract to check that values are tc-results/c and do not contain #f propset or obj
;; Used to contract the return values of typechecking functions.
(define (full-tc-results/c r)
  (match r
    [(tc-any-results: p) (and p #t)]
    [(tc-results: _ ps os)
     (and (andmap (λ (x) x) ps)
          (andmap (λ (x) x) os)
          #t)]
    [(tc-results: _ ps os _ _)
     (and (andmap (λ (x) x) ps)
          (andmap (λ (x) x) os)
          #t)]
    [else #f]))


(define-match-expander tc-result:
  (syntax-rules ()
   [(_ tp fp op) (tc-result tp fp op)]
   [(_ tp) (tc-result tp _ _)]))

;; expand-tc-results: (Listof tc-result) -> (Values (Listof Type) (Listof PropSet) (Listof Object))
(define (expand-tc-results results)
  (values (map tc-result-t results) (map tc-result-pset results) (map tc-result-o results)))

(define-match-expander tc-results:
  (syntax-rules ()
   [(_ tp)
    (tc-results (app expand-tc-results tp _ _) #f)]
   [(_ tp fp op)
    (tc-results (app expand-tc-results tp fp op) #f)]
   [(_ tp fp op dty dbound)
    (tc-results (app expand-tc-results tp fp op) (cons dty dbound))]))

(define-match-expander tc-any-results:
  (syntax-rules ()
   [(_ f)
    (tc-any-results f)]))


(define-match-expander tc-result1:
  (syntax-rules ()
   [(_ tp) (tc-results: (list tp))]
   [(_ tp fp op) (tc-results: (list tp) (list fp) (list op))]))

(define (tc-results-ts* tc)
  (match tc
    [(tc-results: t) t]))

(define-match-expander Result1:
  (syntax-rules ()
   [(_ tp) (Results: (list tp))]
   [(_ tp fp op) (Results: (list tp) (list fp) (list op))]))

;; expand-Results: (Listof Rresult) -> (Values (Listof Type) (Listof PropSet) (Listof Object))
(define (expand-Results results)
  (values (map Result-t results) (map Result-ps results) (map Result-o results)))


(define-match-expander Results:
  (syntax-rules ()
   [(_ tp) (Values: (app expand-Results tp _ _))]
   [(_ tp fp op) (Values: (app expand-Results tp fp op))]
   [(_ tp fp op dty dbound) (ValuesDots: (app expand-Results tp fp op) dty dbound)]))

;; make-tc-result*: Type? PropSet/c Object? -> tc-result?
;; Smart constructor for a tc-result.
(define (-tc-result type [prop -tt-propset] [object -empty-obj])
  (cond
    [(or (equal? type -Bottom) (equal? prop -ff-propset))
     (tc-result -Bottom -ff-propset object)]
    [else
     (tc-result type prop object)]))


;; convenience function for returning the result of typechecking an expression
(define ret
  (case-lambda [(t)
                (make-tc-results
                 (cond [(Type? t)
                        (list (-tc-result t -tt-propset -empty-obj))]
                       [else
                        (for/list ([i (in-list t)])
                          (-tc-result i -tt-propset -empty-obj))])
                 #f)]
               [(t pset)
                (make-tc-results
                 (if (Type? t)
                     (list (-tc-result t pset -empty-obj))
                     (for/list ([i (in-list t)] [pset (in-list pset)])
                       (-tc-result i pset -empty-obj)))
                 #f)]
               [(t pset o)
                (make-tc-results
                 (if (and (list? t) (list? pset) (list? o))
                     (map -tc-result t pset o)
                     (list (-tc-result t pset o)))
                 #f)]
               [(t pset o dty)
                (int-err "ret used with dty without dbound")]
               [(t pset o dty dbound)
                (make-tc-results
                 (if (and (list? t) (list? pset) (list? o))
                     (map -tc-result t pset o)
                     (list (-tc-result t pset o)))
                 (cons dty dbound))]))

(provide/cond-contract
 [ret
  (c:->i ([t (c:or/c Type? (c:listof Type?))])
         ([f (t) (if (list? t)
                     (c:listof (c:or/c #f PropSet?))
                     (c:or/c #f PropSet?))]
          [o (t) (if (list? t)
                     (c:listof (c:or/c #f OptObject?))
                     (c:or/c #f OptObject?))]
          [dty Type?]
          [dbound symbol?])
         [res tc-results/c])])

(define tc-result-equal? equal?)

(provide tc-result: tc-results: tc-any-results: tc-result1: Result1: Results:
         tc-results)
(provide/cond-contract
 [rename -tc-result tc-result
   (c:case->
     (Type? . c:-> . tc-result?)
     (Type? PropSet? OptObject? . c:-> . tc-result?))]
 [tc-any-results ((c:or/c Prop? #f) . c:-> . tc-any-results?)]
 [tc-result-t (tc-result? . c:-> . Type?)]
 [rename tc-results-ts* tc-results-ts (tc-results? . c:-> . (c:listof Type?))]
 [tc-result-equal? (tc-result? tc-result? . c:-> . boolean?)]
 [tc-result? (c:any/c . c:-> . boolean?)]
 [tc-results? (c:any/c . c:-> . boolean?)]
 [tc-results/c c:flat-contract?]
 [tc-results1/c c:flat-contract?]
 [full-tc-results/c c:flat-contract?])
