#lang racket/base

(require "../utils/utils.rkt"
         racket/match (prefix-in - (contract-req))
         racket/format
         (types utils union subtype prop-ops abbrev)
         (utils tc-utils)
         (rep type-rep object-rep prop-rep)
         (typecheck error-message))

(provide/cond-contract
 [check-below (-->i ([s (-or/c Type? full-tc-results/c)]
                     [t (s) (if (Type? s) Type? tc-results/c)])
                    [_ (s) (if (Type? s) Type? full-tc-results/c)])]
 [cond-check-below (-->i ([s (-or/c Type? full-tc-results/c)]
                          [t (s) (-or/c #f (if (Type? s) Type? tc-results/c))])
                         [_ (s) (-or/c #f (if (Type? s) Type? full-tc-results/c))])]
 [fix-results (--> tc-results/c full-tc-results/c)])

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
      [(tc-results: ts) (~a (length ts) " values")]
      ;; TODO simplify this case
      [(tc-results: ts _ _ dty _) (~a (length ts) " " (if (= (length ts) 1) "value" "values")
                                      " and `" dty " ...'")]
      [(tc-any-results: _) "unknown number"]))
  (type-mismatch
    (value-string expected) (value-string actual)
    "mismatch in number of values"))

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
    [(tc-any-results: f) (tc-any-results (fix-props f -tt))]
    [(tc-results: ts ps os)
     (ret ts (map fix-props ps) (map fix-object os))]
    [(tc-results: ts ps os dty dbound)
     (ret ts (map fix-props ps) (map fix-object os) dty dbound)]))

(define (fix-results/bottom r)
  (match r
    [(tc-any-results: f) (tc-any-results (fix-props f -ff))]
    [(tc-results: ts ps os)
     (ret ts (for/list ([p ps]) (fix-props p -ff-propset)) (map fix-object os))]
    [(tc-results: ts ps os dty dbound)
     (ret ts (for/list ([p ps]) (fix-props p -ff-propset)) (map fix-object os) dty dbound)]))



;; check-below : (/\ (Results Type -> Result)
;;                   (Results Results -> Result)
;;                   (Type Results -> Type)
;;                   (Type Type -> Type))
(define (check-below tr1 expected)
  (define (prop-set-better? p1 p2)
    (match* (p1 p2)
      [(p p) #t]
      [(p #f) #t]
      [((PropSet: p1+ p1-) (PropSet: p2+ p2-))
       (and (implies-atomic? p1+ p2+)
            (implies-atomic? p1- p2-))]
      [(_ _) #f]))
  (define (object-better? o1 o2)
    (match* (o1 o2)
      [(o o) #t]
      [(o (or #f (Empty:))) #t]
      [(_ _) #f]))
  (define (prop-better? p1 p2)
    (or (not p2)
        (implies-atomic? p1 p2)))

  (match* (tr1 expected)
    ;; This case has to be first so that bottom (exceptions, etc.) can be allowed in cases
    ;; where multiple values are expected.
    ;; We can ignore the props and objects in the actual value because they would never be about a value
    [((tc-result1: (? (lambda (t) (type-equal? t (Un))))) _)
     (fix-results/bottom expected)]

    [((tc-any-results: p1) (tc-any-results: p2))
     (unless (prop-better? p1 p2)
       (type-mismatch p2 p1 "mismatch in proposition"))
     (tc-any-results (fix-props p2 p1))]

    [((or (tc-results: _ (list (PropSet: fs+ fs-) ...) _)
          (tc-results: _ (list (PropSet: fs+ fs-) ...) _ _ _))
      (tc-any-results: p2))
     (define merged-prop (apply -and (map -or fs+ fs-)))
     (unless (prop-better? merged-prop p2)
       (type-mismatch p2 merged-prop "mismatch in proposition"))
     (tc-any-results (fix-props p2 merged-prop))]


    [((tc-result1: t1 p1 o1) (tc-result1: t2 p2 o2))
     (cond
       [(not (subtype t1 t2))
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
     (ret t2 (fix-props p2 p1) (fix-object o2 o1))]

    ;; case where expected is like (Values a ... a) but got something else
    [((tc-results: t1 p1 o1) (tc-results: t2 p2 o2 dty dbound))
     (value-mismatch expected tr1)
     (fix-results expected)]

    ;; case where you have (Values a ... a) but expected something else
    [((tc-results: t1 p1 o1 dty dbound) (tc-results: t2 p2 o2))
     (value-mismatch expected tr1)
     (fix-results expected)]

    [((tc-results: t1 p1 o1 dty1 dbound)
      (tc-results: t2 (list (or #f (PropSet: (TrueProp:) (TrueProp:))) ...)
                      (list (or #f (Empty:)) ...) dty2 dbound))
     (cond
       [(= (length t1) (length t2))
        (unless (andmap subtype t1 t2)
          (expected-but-got (stringify t2) (stringify t1)))
        (unless (subtype dty1 dty2)
          (type-mismatch dty2 dty1 "mismatch in ... argument"))]
       [else
         (value-mismatch expected tr1)])
     (fix-results expected)]

    [((tc-results: t1 p1 o1 dty1 dbound) (tc-results: t2 p2 o2 dty2 dbound))
     (cond
       [(= (length t1) (length t2))
        (unless (andmap subtype t1 t2)
          (expected-but-got (stringify t2) (stringify t1)))
        (unless (subtype dty1 dty2)
          (type-mismatch dty2 dty1 "mismatch in ... argument"))]
       [else
         (value-mismatch expected tr1)])
     (fix-results expected)]

    [((tc-results: t1 p1 o1)
      (tc-results: t2 (list (or #f (PropSet: (TrueProp:) (TrueProp:))) ...)
                      (list (or #f (Empty:)) ...)))
     (unless (= (length t1) (length t2))
       (value-mismatch expected tr1))
     (unless (for/and ([t (in-list t1)] [s (in-list t2)]) (subtype t s))
       (expected-but-got (stringify t2) (stringify t1)))
     (fix-results expected)]

    [((tc-results: t1 fs os) (tc-results: t2 fs os))
     (unless (= (length t1) (length t2))
       (value-mismatch expected tr1))
     (unless (for/and ([t (in-list t1)] [s (in-list t2)]) (subtype t s))
       (expected-but-got (stringify t2) (stringify t1)))
     (fix-results expected)]

    [((tc-results: t1 p1 o1) (tc-results: t2 p2 o2)) (=> continue)
     (if (= (length t1) (length t2))
         (continue)
         (value-mismatch expected tr1))
     (fix-results expected)]

    [((tc-any-results: _) (? tc-results?))
     (value-mismatch expected tr1)
     (fix-results expected)]

    [((? Type? t1) (? Type? t2))
     (unless (subtype t1 t2)
       (expected-but-got t2 t1))
     expected]
    [((tc-results: ts fs os dty dbound) (tc-results: ts* fs* os* dty* dbound*))
     (int-err "dotted types with different bounds/propositions/objects in check-below nyi: ~a ~a" tr1 expected)]
    [(a b) (int-err "unexpected input for check-below: ~a ~a" a b)]))
