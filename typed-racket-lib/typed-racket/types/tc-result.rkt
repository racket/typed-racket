#lang racket/base

(require "../utils/utils.rkt"
         (rep rep-utils core-rep type-rep prop-rep
              values-rep free-variance)
         (utils tc-utils)
         (types base-abbrev)
         racket/match
         (prefix-in c: (contract-req)))

;; this structure represents the result of typechecking an expression
;; fields are #f only when the direct result of parsing or annotations
(def-rep tc-result ([t Type?]
                    [pset (c:or/c PropSet? #f)]
                    [o (c:or/c OptObject? #f)])
  #:no-provide
  [#:frees (f)
   (combine-frees (list (f t)
                        (if pset (f pset) empty-free-vars)
                        (if o (f o) empty-free-vars)))]
  [#:fmap (f)
   (make-tc-result (f t)
                   (and pset (f pset))
                   (and o (f o)))]
  [#:for-each (f)
   (f t)
   (when pset (f pset))
   (when o (f o))])

(def-rep tc-results ([tcrs (c:listof tc-result?)]
                     [drst (c:or/c #f RestDots?)])
  #:no-provide
  [#:frees (f)
   (combine-frees (cons (if drst (f drst) empty-free-vars)
                        (map f tcrs)))]
  [#:fmap (f)
   (make-tc-results (map f tcrs)
                    (and drst (f drst)))]
  [#:for-each (f)
   (for-each f tcrs)
   (when drst (f drst))])

(def-rep tc-any-results ([p (c:or/c Prop? #f)])
  #:no-provide
  [#:frees (f) (if p (f p) empty-free-vars)]
  [#:fmap (f) (make-tc-any-results (and p (f p)))]
  [#:for-each (f) (when p (f p))])

(define (tc-results/c v)
  (or (tc-results? v)
      (tc-any-results? v)))

(define (tc-results1/c v)
  (and (tc-results? v)
       (= (length (tc-results-tcrs v)) 1)))

;; Contract to check that values are tc-results/c and do not contain #f propset or obj
;; Used to contract the return values of typechecking functions.
(define (full-tc-results/c r)
  (match r
    [(tc-any-results: p) (and p #t)]
    [(tc-results: (list (tc-result: _ pss os) ...) _)
     (and (andmap (λ (x) x) pss)
          (andmap (λ (x) x) os)
          #t)]
    [_ #f]))


(define-match-expander tc-result:*
  (syntax-rules ()
   [(_ tp fp op) (tc-result tp fp op)]
   [(_ tp) (tc-result tp _ _)]))


(define-match-expander tc-result1:
  (syntax-rules ()
   [(_ t) (tc-results: (list (tc-result: t _ _)) #f)]
   [(_ t ps o) (tc-results: (list (tc-result: t ps o)) #f)]))

(define (tc-results-ts* tc)
  (match tc
    [(tc-results: (list (tc-result: ts _ _) ...) _) ts]))

(define-match-expander Result1:
  (syntax-rules ()
   [(_ tp) (Results: (list tp))]
   [(_ tp fp op) (Results: (list tp) (list fp) (list op))]))

;; make-tc-result*: Type? PropSet/c Object? -> tc-result?
;; Smart constructor for a tc-result.
(define (-tc-result type [prop -tt-propset] [object -empty-obj])
  (cond
    [(or (equal? type -Bottom) (equal? prop -ff-propset))
     (make-tc-result -Bottom -ff-propset object)]
    [else
     (make-tc-result type prop object)]))


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
                 (make-RestDots dty dbound))]))


;; fix-props:
;;  PropSet [PropSet] -> PropSet
;;    or
;;  Prop [Prop]       -> Prop
;; Turns #f prop/propset into the actual prop; leaves other props alone.
(define (fix-props p1 [p2 -tt-propset])
  (or p1 p2))

;; fix-object: Object [Object] -> Object
;; Turns #f into the actual object; leaves other objects alone.
(define (fix-object o1 [o2 -empty-obj])
  (or o1 o2))

;; fix-results: tc-results -> tc-results
;; Turns #f Prop or Obj into the Empty/Trivial
(define (fix-results r)
  (match r
    [(tc-any-results: p) (make-tc-any-results (fix-props p -tt))]
    [(tc-results: ts drst)
     (make-tc-results
      (map (match-lambda
             [(tc-result: t ps o)
              (make-tc-result t (fix-props ps) (fix-object o))])
           ts)
      drst)]))

(define (fix-results/bottom r)
  (match r
    [(tc-any-results: p) (make-tc-any-results (fix-props p -ff))]
    [(tc-results: (list (tc-result: ts ps os) ...) #f)
     (ret ts
          (for/list ([p (in-list ps)]) (fix-props p -ff-propset))
          (map fix-object os))]
    [(tc-results: (list (tc-result: ts ps os) ...) (RestDots: dty dbound))
     (ret ts
          (for/list ([p (in-list ps)]) (fix-props p -ff-propset))
          (map fix-object os)
          dty
          dbound)]))

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

(provide tc-any-results: tc-result1: Result1: tc-results:
         (rename-out [tc-result:* tc-result:]))
(provide/cond-contract
 [-tc-result
  (c:case->
   (Type? . c:-> . tc-result?)
   (Type? PropSet? OptObject? . c:-> . tc-result?))]
 [tc-result-t (tc-result? . c:-> . Type?)]
 [rename make-tc-results -tc-results
         (c:-> (c:listof tc-result?)
               (c:or/c #f RestDots?)
               tc-results?)]
 [rename make-tc-any-results -tc-any-results
         (c:-> (c:or/c #f Prop?) tc-any-results?)]
 [rename tc-results-ts* tc-results-ts (tc-results? . c:-> . (c:listof Type?))]
 [tc-result-equal? (tc-result? tc-result? . c:-> . boolean?)]
 [tc-result? (c:any/c . c:-> . boolean?)]
 [tc-results? (c:any/c . c:-> . boolean?)]
 [tc-results/c c:flat-contract?]
 [tc-results1/c c:flat-contract?]
 [full-tc-results/c c:flat-contract?]
 [fix-results (c:-> tc-results/c full-tc-results/c)]
 [fix-results/bottom (c:-> tc-results/c full-tc-results/c)]
 [fix-props
  (c:->* ((c:or/c #f Prop? PropSet?))
         ((c:or/c Prop? PropSet?))
         (c:or/c Prop? PropSet?))]
 [fix-object (c:->* ((c:or/c #f OptObject?)) (OptObject?) OptObject?)])
