#lang racket/base

(require "../utils/utils.rkt"
         racket/dict
         racket/match (prefix-in - (contract-req))
         racket/format
         racket/set
         (types utils union subtype filter-ops abbrev resolve)
         (utils tc-utils)
         (rep type-rep object-rep filter-rep)
         (only-in (types printer) pretty-format-type))

(provide/cond-contract
 [check-below (-->i ([s (-or/c Type/c full-tc-results/c)]
                     [t (s) (if (Type/c? s) Type/c tc-results/c)])
                    [_ (s) (if (Type/c? s) Type/c full-tc-results/c)])]
 [cond-check-below (-->i ([s (-or/c Type/c full-tc-results/c)]
                          [t (s) (-or/c #f (if (Type/c? s) Type/c tc-results/c))])
                         [_ (s) (-or/c #f (if (Type/c? s) Type/c full-tc-results/c))])]
 [fix-results (--> tc-results/c full-tc-results/c)]
 [type-mismatch (-->* ((-or/c Type/c string?) (-or/c Type/c string?))
                      ((-or/c string? #f))
                      -any)])

(define (print-object o)
  (match o
    [(or (NoObject:) (Empty:)) "no object"]
    [_ (format "object ~a" o)]))

;; If expected is #f, then just return tr1
;; else behave as check-below
(define (cond-check-below tr1 expected)
  (if expected (check-below tr1 expected) tr1))

;; type-mismatch : Any Any [String] -> Void
;; Type errors with "type mismatch", arguments may be types or other things
;; like the length of a list of types
(define (type-mismatch t1 t2 [more #f])
  (define t1* (if (Type/c? t1) (pretty-format-type t1 #:indent 12) t1))
  (define t2* (if (Type/c? t2) (pretty-format-type t2 #:indent 9) t2))
  (tc-error/fields "type mismatch" #:more more "expected" t1* "given" t2* #:delayed? #t))

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

;; expected-but-got : (U Type String) (U Type String) -> Void
;;
;; Helper to print messages of the form
;;   "Expected a, but got b"
;;
;; Also handles special cases like when two type variables
;; have the same name but are different. Or for types that are too
;; long for a subtyping error to be helpful directly.
(define (expected-but-got t1 t2)
  (define r1 (resolve t1))
  (define r2 (resolve t2))
  (match* (r1 r2)
    [((F: s1) (F: s2))
     (=> fail)
     (unless (string=? (symbol->string s1) (symbol->string s2))
       (fail))
     ;; FIXME: this case could have a better error message that, say,
     ;;        prints the binding locations of each type variable.
     (type-mismatch (format "`~a'" t1) (format "a different `~a'" t2)
                    "type variables bound in different scopes")]
    [((? Class?) (? Class?))
     (class-mismatch r1 r2)]
    [((Instance: c1) (Instance: c2))
     (define r1 (resolve c1))
     (define r2 (resolve c2))
     (class-mismatch r1 r2 #t)]
    ;; Don't call this with resolved types since we may want to print
    ;; the type alias name instead of the actual type
    [(_ _) (type-mismatch t1 t2)]))

;; class-mismatch : Class Class Boolean -> Void
;; Explains an expected/given type mismatch for cases with Class or Instance
;; types. In both cases, the Class type is passed in to generate the error
;; message (the object? argument distinguishes the cases).
(define (class-mismatch c1 c2 [object? #f])
  (define class/object (if object? "object" "class"))
  (match-define (Class: row inits fields methods augments init-rest) c1)
  (match-define (Class: row* inits* fields* methods* augments* init-rest*) c2)
  (when (not object?)
    (when (and (F? row) (not (F? row*)))
      (type-mismatch (format "Class with row variable `~a'" row)
                     (format "Class with no row variable")))
    (when (and (F? row*) (not (F? row)))
      (type-mismatch (format "Class with no row variable")
                     (format "Class with row variable `~a'" row*)))
    (when (and (F? row) (F? row) (not (equal? row row*)))
      (type-mismatch (format "Class with row variable `~a'" row)
                     (format "Class with row variable `~a'" row*))))
  (define (missing-key kind map1 map2)
    (define keys1 (map car map1))
    (define keys2 (map car map2))
    (cond [(not (set-empty? (set-subtract keys1 keys2)))
           (define key (set-first (set-subtract keys1 keys2)))
           (tc-error/expr/fields "type mismatch"
                                 #:more
                                 (format "~a lacks expected ~a `~a'" class/object kind key)
                                 #:return #f)]
          [(and (not object?)
                (not (set-empty? (set-subtract keys2 keys1))))
           (define key (set-first (set-subtract keys2 keys1)))
           (tc-error/expr/fields "type mismatch"
                                 #:more
                                 (format "class has ~a `~a' that is not in expected type" kind key)
                                 #:return #f)]
          [;; init arguments out of order
           (and (equal? kind "init")
                (not object?)
                (set=? keys1 keys2)
                (not (equal? keys1 keys2)))
           (tc-error/expr/fields "type mismatch"
                                 #:more
                                 "mismatch in initialization argument order"
                                 "expected" keys1
                                 "given" keys2
                                 #:return #f)]
          [else #t]))
  (define (subtype-clauses kind map1 map2)
    (define keys1 (map car map1))
    (define keys2 (map car map2))
    (define both-keys (set-intersect keys1 keys2))
    (for/and ([key (in-list both-keys)])
      (define entry1 (dict-ref map1 key))
      (define entry2 (dict-ref map2 key))
      (cond [;; a field or method
             (null? (cdr entry1))
             (or (subtype (car entry2) (car entry1))
                 (tc-error/expr/fields "type mismatch"
                                       #:more (format "wrong type for ~a `~a'" kind key)
                                       "expected" (car entry1)
                                       "given" (car entry2)
                                       #:return #f))]
            [;; an init arg
             else
             (match-define (list type1 optional?1) entry1)
             (match-define (list type2 optional?2) entry2)
             (and (or (subtype type2 type1)
                      (tc-error/expr/fields "type mismatch"
                                            #:more (format "wrong type for ~a `~a'" kind key)
                                            "expected" (car entry1)
                                            "given" (car entry2)
                                            #:return #f))
                  (or (equal? optional?1 optional?2)
                      (tc-error/expr/fields "type mismatch"
                                            "expected"
                                            (format "~a init `~a'"
                                                    (if optional?1 "optional" "mandatory")
                                                    key)
                                            "given"
                                            (format "~a init `~a'"
                                                    (if optional?2 "optional" "mandatory")
                                                    key)
                                            #:return #f)))])))
  (define (check-init-rest ir1 ir2)
    (and (or (not (and ir1 (not ir2)))
             (tc-error/expr/fields "type mismatch"
                                   "expected" "Class with init-rest type"
                                   "given" "Class with no init-rest type"
                                   #:return #f))
         (or (not (and ir2 (not ir1)))
             (tc-error/expr/fields "type mismatch"
                                   "expected" "Class with no init-rest type"
                                   "given" "Class with init-rest type"
                                   #:return #f))
         (or (not (and ir1 ir2))
             (subtype ir2 ir1)
             (tc-error/expr/fields "type mismatch"
                                   #:more "wrong type for init-rest clause"
                                   "expected" ir1
                                   "given" ir2
                                   #:return #f))))
  (and (or object?
           (and (missing-key "init" inits inits*)
                (missing-key "augmentable method" augments augments*)
                (subtype-clauses "init" inits inits*)
                (subtype-clauses "augmentable method" augments augments*)
                (check-init-rest init-rest init-rest*)))
       (missing-key "method" methods methods*)
       (missing-key "field" fields fields*)
       (subtype-clauses "method" methods methods*)
       (subtype-clauses "field" fields fields*)))

;; fix-filter: FilterSet [FilterSet] -> FilterSet
;; Turns NoFilter into the actual filter; leaves other filters alone.
(define (fix-filter f [f2 -top-filter])
  (match f
    [(NoFilter:) f2]
    [else f]))

;; fix-object: Object [Object] -> Object
;; Turns NoObject into the actual object; leaves other objects alone.
(define (fix-object o [o2 -empty-obj])
  (match o
    [(NoObject:) o2]
    [else o]))

;; fix-results: tc-results -> tc-results
;; Turns NoObject/NoFilter into the Empty/TopFilter
(define (fix-results r)
  (match r
    [(tc-any-results: f) (tc-any-results (fix-filter f -top))]
    [(tc-results: ts fs os)
     (ret ts (map fix-filter fs) (map fix-object os))]
    [(tc-results: ts fs os dty dbound)
     (ret ts (map fix-filter fs) (map fix-object os) dty dbound)]))

(define (fix-results/bottom r)
  (match r
    [(tc-any-results: f) (tc-any-results (fix-filter f -bot))]
    [(tc-results: ts fs os)
     (ret ts (for/list ([f fs]) (fix-filter f -bot-filter)) (map fix-object os))]
    [(tc-results: ts fs os dty dbound)
     (ret ts (for/list ([f fs]) (fix-filter f -bot-filter)) (map fix-object os) dty dbound)]))



;; check-below : (/\ (Results Type -> Result)
;;                   (Results Results -> Result)
;;                   (Type Results -> Type)
;;                   (Type Type -> Type))
(define (check-below tr1 expected)
  (define (filter-set-better? f1 f2)
    (match* (f1 f2)
      [(f f) #t]
      [(f (NoFilter:)) #t]
      [((FilterSet: f1+ f1-) (FilterSet: f2+ f2-))
       (and (implied-atomic? f2+ f1+)
            (implied-atomic? f2- f1-))]
      [(_ _) #f]))
  (define (object-better? o1 o2)
    (match* (o1 o2)
      [(o o) #t]
      [(o (or (NoObject:) (Empty:))) #t]
      [(_ _) #f]))
  (define (filter-better? f1 f2)
    (or (NoFilter? f2)
        (implied-atomic? f2 f1)))

  (match* (tr1 expected)
    ;; This case has to be first so that bottom (exceptions, etc.) can be allowed in cases
    ;; where multiple values are expected.
    ;; We can ignore the filters and objects in the actual value because they would never be about a value
    [((tc-result1: (? (lambda (t) (type-equal? t (Un))))) _)
     (fix-results/bottom expected)]

    [((tc-any-results: f1) (tc-any-results: f2))
     (unless (filter-better? f1 f2)
       (type-mismatch f2 f1 "mismatch in filter"))
     (tc-any-results (fix-filter f2 f1))]

    [((or (tc-results: _ (list (FilterSet: fs+ fs-) ...) _)
          (tc-results: _ (list (FilterSet: fs+ fs-) ...) _ _ _))
      (tc-any-results: f2))
     (define merged-filter (apply -and (map -or fs+ fs-)))
     (unless (filter-better? merged-filter f2)
       (type-mismatch f2 merged-filter "mismatch in filter"))
     (tc-any-results (fix-filter f2 merged-filter))]


    [((tc-result1: t1 f1 o1) (tc-result1: t2 f2 o2))
     (cond
       [(not (subtype t1 t2))
        (expected-but-got t2 t1)]
       [(and (not (filter-set-better? f1 f2))
             (object-better? o1 o2))
        (type-mismatch f2 f1 "mismatch in filter")]
       [(and (filter-set-better? f1 f2)
             (not (object-better? o1 o2)))
        (type-mismatch (print-object o2) (print-object o1) "mismatch in object")]
       [(and (not (filter-set-better? f1 f2))
             (not (object-better? o1 o2)))
        (type-mismatch (format "`~a' and `~a'" f2 (print-object o2))
                       (format "`~a' and `~a'" f1 (print-object o1))
                       "mismatch in filter and object")])
     (ret t2 (fix-filter f2 f1) (fix-object o2 o1))]

    ;; case where expected is like (Values a ... a) but got something else
    [((tc-results: t1 f1 o1) (tc-results: t2 f2 o2 dty dbound))
     (value-mismatch expected tr1)
     (fix-results expected)]

    ;; case where you have (Values a ... a) but expected something else
    [((tc-results: t1 f1 o1 dty dbound) (tc-results: t2 f2 o2))
     (value-mismatch expected tr1)
     (fix-results expected)]

    [((tc-results: t1 f1 o1 dty1 dbound)
      (tc-results: t2 (list (or (NoFilter:) (FilterSet: (Top:) (Top:))) ...)
                      (list (or (NoObject:) (Empty:)) ...) dty2 dbound))
     (cond
       [(= (length t1) (length t2))
        (unless (andmap subtype t1 t2)
          (expected-but-got (stringify t2) (stringify t1)))
        (unless (subtype dty1 dty2)
          (type-mismatch dty2 dty1 "mismatch in ... argument"))]
       [else
         (value-mismatch expected tr1)])
     (fix-results expected)]

    [((tc-results: t1 f1 o1 dty1 dbound) (tc-results: t2 f2 o2 dty2 dbound))
     (cond
       [(= (length t1) (length t2))
        (unless (andmap subtype t1 t2)
          (expected-but-got (stringify t2) (stringify t1)))
        (unless (subtype dty1 dty2)
          (type-mismatch dty2 dty1 "mismatch in ... argument"))]
       [else
         (value-mismatch expected tr1)])
     (fix-results expected)]

    [((tc-results: t1 f1 o1)
      (tc-results: t2 (list (or (NoFilter:) (FilterSet: (Top:) (Top:))) ...)
                      (list (or (NoObject:) (Empty:)) ...)))
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

    [((tc-results: t1 f1 o1) (tc-results: t2 f2 o2)) (=> continue)
     (if (= (length t1) (length t2))
         (continue)
         (value-mismatch expected tr1))
     (fix-results expected)]

    [((tc-any-results: _) (? tc-results?))
     (value-mismatch expected tr1)
     (fix-results expected)]

    [((? Type/c? t1) (? Type/c? t2))
     (unless (subtype t1 t2)
       (expected-but-got t2 t1))
     expected]
    [((tc-results: ts fs os dty dbound) (tc-results: ts* fs* os* dty* dbound*))
     (int-err "dotted types with different bounds/filters/objects in check-below nyi: ~a ~a" tr1 expected)]
    [(a b) (int-err "unexpected input for check-below: ~a ~a" a b)]))
