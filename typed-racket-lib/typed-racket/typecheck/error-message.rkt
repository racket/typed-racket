#lang racket/base

;; This module provides helpers for producing type error messages
;; for check-below.

(require "../utils/utils.rkt"
         (prefix-in - (contract-req))
         racket/dict
         racket/match
         racket/set
         racket/format
         (types utils subtype resolve)
         (utils tc-utils)
         (rep type-rep)
         (only-in (types printer) pretty-format-rep))

(provide/cond-contract [expected-but-got
                        (--> (-or/c Type? string?)
                             (-or/c Type? string?)
                             -any)]
                       [type-mismatch
                        (-->* ((-or/c Type? Prop? PropSet? string?)
                               (-or/c Type? Prop? PropSet? string?))
                              ((-or/c string? #f))
                              -any)])

;; type-mismatch : Any Any [String] -> Void
;; Type errors with "type mismatch", arguments may be types or other things
;; like the length of a list of types
(define (type-mismatch t1 t2 [more #f])
  (define t1* (if (Type? t1) (pretty-format-rep t1 #:indent 12) t1))
  (define t2* (if (Type? t2) (pretty-format-rep t2 #:indent 9) t2))
  (tc-error/fields "type mismatch" #:more more "expected" t1* "given" t2* #:delayed? #t))

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
     #:when (string=? (symbol->string s1) (symbol->string s2))
     ;; FIXME: this case could have a better error message that, say,
     ;;        prints the binding locations of each type variable.
     (type-mismatch (format "`~a'" t1) (format "a different `~a'" t2)
                    "type variables bound in different scopes")]
    [((Struct: n1 _ _ _ _ _ _) (Struct: n2 _ _ _ _ _ _))
     #:when (and (not (free-identifier=? n1 n2))
                 (eq? (syntax-e n1) (syntax-e n2)))
     (type-mismatch (syntax-e n1) (format "a different ~a" (syntax-e n2))
                    "incompatible struct types with the same name")]
    [((? Class?) (? Class?))
     (class-mismatch r1 r2)]
    [((Instance: (app resolve (? Class? c1))) (Instance: (app resolve (? Class? c2))))
     (class-mismatch c1 c2 #t)]
    [((Has-Struct-Property: prop-name) (Struct: n2 _ _ _ _ _ _))
     (type-mismatch t1 t2 (format "struct ~a doesn't have the property ~a"
                                  (syntax-e n2)
                                  (syntax-e prop-name)))]
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
