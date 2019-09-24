#lang racket/base

(require "../utils/utils.rkt"
         racket/match (prefix-in - (contract-req))
         racket/format
         (env lexical-env)
         (types utils subtype prop-ops abbrev tc-result)
         (utils tc-utils)
         (rep type-rep object-rep prop-rep)
         (typecheck error-message tc-envops))

(provide/cond-contract
 [check-below (-->i ([s (t) (if (Type? t)
                                (-or/c full-tc-results/c Type?)
                                full-tc-results/c)]
                     [t (-or/c Type? tc-results/c)])
                    [_ (t) (if (Type? t) Type? full-tc-results/c)])]
 [cond-check-below (-->i ([s (-or/c Type? full-tc-results/c)]
                          [t (s) (-or/c #f (if (Type? s) Type? tc-results/c))])
                         [_ (s) (-or/c #f (if (Type? s) Type? full-tc-results/c))])])

(provide type-mismatch)

(define (print-object o)
  (match o
    [(or #f (Empty:)) "no object"]
    [_ (format "object ~a" o)]))

;; If expected is #f, then just return tr1
;; else behave as check-below
(define (cond-check-below tr1 expected)
  (if expected (check-below tr1 expected) tr1))

;; value-mismatch : tc-results/c tc-results/c -> void?
;; Helper to print messages of the form
;;   "Expecte n values, but got m values"
(define (value-mismatch expected actual)
  (define (value-string ty)
    (match ty
      [(tc-result1: _) "1 value"]
      [(tc-results: tcrs #f) (~a (length tcrs) " values")]
      ;; TODO simplify this case
      [(tc-results: tcrs (RestDots: dty _))
       (~a (length tcrs) " " (if (= (length tcrs) 1) "value" "values")
           " and `" dty " ...'")]
      [(tc-any-results: _) "unknown number"]))
  (type-mismatch
    (value-string expected) (value-string actual)
    "mismatch in number of values"))


;; check-below : (/\ (Results Type -> Result)
;;                   (Results Results -> Result)
;;                   (Type Type -> Type))
(define (check-below tr1 expected)
  (define (prop-set-better? p1 p2)
    (match* (p1 p2)
      [(p p) #t]
      [(p #f) #t]
      [((PropSet: p1+ p1-) (PropSet: p2+ p2-))
       (define positive-implies?
         (or (TrueProp? p2+)
             (FalseProp? p1+)
             (implies-in-env? (lexical-env) p1+ p2+)))
       (and positive-implies?
            (or (TrueProp? p2-)
                (FalseProp? p1-)
                (implies-in-env? (lexical-env) p1- p2-)))]
      [(_ _) #f]))
  (define (object-better? o1 o2)
    (match* (o1 o2)
      [(o o) #t]
      [(o (or #f (Empty:))) #t]
      [(_ _) #f]))
  (define (prop-better? p1 p2)
    (or (not p2)
        (implies? p1 p2)))
  (define (RestDots-better? rdots1 rdots2)
    (match* (rdots1 rdots2)
      [(rd rd) #t]
      [((RestDots: dty1 dbound)
        (RestDots: dty2 dbound))
       (subtype dty1 dty2)]
      [(_ _) #f]))

  (match* (tr1 expected)

    [((tc-result1: t1 p1 o1) (? Type? t2))
     (cond
       [(with-refinements?)
        (with-naively-extended-lexical-env
            [#:props (list (-is-type o1 t1)
                           (-or (PropSet-thn p1) (PropSet-els p1)))]
          (unless (subtype t1 t2 o1)
            (expected-but-got t2 t1)))]
       [else (unless (subtype t1 t2 o1)
               (expected-but-got t2 t1))])
     t2]
    ;; This case has to be first so that bottom (exceptions, etc.) can be allowed in cases
    ;; where multiple values are expected.
    ;; We can ignore the props and objects in the actual value because they would never be about a value
    [((tc-result1: (? Bottom?)) _)
     (fix-results/bottom expected)]

    [((tc-any-results: p1) (tc-any-results: p2))
     (unless (prop-better? p1 p2)
       (type-mismatch p2 p1 "mismatch in proposition"))
     (-tc-any-results (fix-props p2 p1))]

    [((tc-results: tcrs _)
      (tc-any-results: p2))
     (define merged-prop
       (apply -and
              (map (match-lambda
                     [(tc-result: _ (PropSet: p+ p-) _) (-or p+ p-)])
                   tcrs)))
     (unless (prop-better? merged-prop p2)
       (type-mismatch p2 merged-prop "mismatch in proposition"))
     (-tc-any-results (fix-props p2 merged-prop))]

    [((tc-result1: t1 p1 o1) (tc-result1: t2 p2 o2))
     (define (perform-check!)
       (cond
         [(not (subtype t1 t2 o1))
          (expected-but-got t2 t1)]
         [(and (not (prop-set-better? p1 p2))
               (object-better? o1 o2))
          (type-mismatch p2 p1 "mismatch in proposition")]
         [(and (prop-set-better? p1 p2)
               (not (object-better? o1 o2)))
          (type-mismatch (print-object o2) (print-object o1) "mismatch in object")]
         [(and (not (prop-set-better? p1 p2))
               (not (object-better? o1 o2)))
          (type-mismatch (format "`~a' and `~a'" p2 (print-object o2))
                         (format "`~a' and `~a'" p1 (print-object o1))
                         "mismatch in proposition and object")])
       (ret t2 (fix-props p2 p1) (fix-object o2 o1)))
     (cond
       [(with-refinements?)
        (with-naively-extended-lexical-env
            [#:props (list (-is-type o1 t1)
                           (-or (PropSet-thn p1) (PropSet-els p1)))]
          (perform-check!))]
       [else (perform-check!)])]

    ;; case where expected is like (Values a ... a) but got something else
    [((tc-results: _ #f) (tc-results: _ (? RestDots?)))
     (value-mismatch expected tr1)
     (fix-results expected)]

    ;; case where you have (Values a ... a) but expected something else
    [((tc-results: _ (? RestDots?)) (tc-results: _ #f))
     (value-mismatch expected tr1)
     (fix-results expected)]

    ;; case where both have no '...', or both have '...'
    [((tc-results: tcrs1 db1)
      (tc-results: tcrs2 db2))
     (cond
       [(= (length tcrs1) (length tcrs2))
        (unless (andmap (match-lambda**
                         [((tc-result: t1 ps1 o1)
                           (tc-result: t2 ps2 o2))
                          (and (subtype t1 t2 o1)
                               (prop-set-better? ps1 ps2)
                               (object-better? o1 o2))])
                        tcrs1
                        tcrs2)
          (expected-but-got (stringify (map tc-result-t tcrs1))
                            (stringify (map tc-result-t tcrs2))))
        (match* (db1 db2)
          [((RestDots: dty1 dbound1)
            (RestDots: dty2 dbound2))
           #:when (not (and (eq? dbound1 dbound2)
                            (subtype dty1 dty2)))
           (type-mismatch dty2 dty1 "mismatch in ... argument")]
          [(_ _) (void)])]
       [else
        (value-mismatch expected tr1)])
     (fix-results expected)]

    [((tc-any-results: _) (? tc-results?))
     (value-mismatch expected tr1)
     (fix-results expected)]

    [((? Type? t1) (? Type? t2))
     (unless (subtype t1 t2)
       (expected-but-got t2 t1))
     expected]

    [(a b) (int-err "unexpected input for check-below: ~a ~a" a b)]))
