#lang typed/racket

;; Test prefab struct declarations

(provide (struct-out foo))

(struct foo ([x : Symbol]) #:prefab)
(struct bar foo ([y : String] [z : String]) #:prefab)
(define-struct foo* ([x : Symbol]) #:prefab)
(define-struct (bar* foo*) ([y : String] [z : String]) #:prefab)

(: a-bar (Prefab (bar foo 1) Symbol String String))
(define a-bar (bar 'foo "bar1" "bar2"))

(ann (foo-x (foo 'foo)) Symbol)
(ann (bar-y (bar 'foo "bar1" "bar2")) String)
(ann (foo-x #s(foo "hi")) String)
(ann (foo-x #s((bar foo 1) #t 42 "!")) True)
(ann (bar-y #s((bar foo 1) #t 42 "!")) Number)
(ann (bar-z #s((bar foo 1) #t 42 "!")) String)

(foo*-x (make-foo* 'foo))
(bar*-y (make-bar* 'foo "bar1" "bar2"))

;; prefab keys may be normalized or not
(: a-bar-2 (Prefab (bar 2 foo 1 (0 #f) #()) Symbol String String))
(define a-bar-2 (bar 'foo "bar1" "bar2"))

;; prefab subtyping is computed via the key and field length
(: a-bar-3 (Prefab foo Symbol))
(define a-bar-3 (bar 'foo "bar1" "bar2"))


(: read-some-unknown-foo1 (-> (PrefabTop foo 1) Any))
(define (read-some-unknown-foo1 f)
  (foo-x f))

(: read-some-unknown-foo2 (-> (Prefab foo Any) Any))
(define (read-some-unknown-foo2 f)
  (foo-x f))


(ann read-some-unknown-foo1 (-> (Prefab foo Any) Any))

(ann read-some-unknown-foo2 (-> (PrefabTop foo 1) Any))


(: read-some-unknown-foo3 (-> Any Any))
(define (read-some-unknown-foo3 x)
  (if (foo? x)
      (read-some-unknown-foo1 x)
      42))


(: read-some-unknown-foo4 (-> Any Any))
(define (read-some-unknown-foo4 x)
  (if (bar? x)
      (read-some-unknown-foo1 x)
      42))

(define-predicate foo/sym1? foo)
(: read-known-foo1 (-> Any Symbol))
(define (read-known-foo1 f)
  (if (foo/sym1? f)
      (foo-x f)
      'other))

(: foo/sym2? (-> Any Boolean : foo))
(define (foo/sym2? x)
  (and (foo? x) (symbol? (foo-x x))))

(: read-known-foo2 (-> Any Symbol))
(define (read-known-foo2 f)
  (if (foo/sym2? f)
      (foo-x f)
      'other))

(: bar/str-str? (-> Any Boolean : bar))
(define (bar/str-str? x)
  (and (foo? x)
       (symbol? (foo-x x))
       (bar? x)
       (string? (bar-y x))
       (string? (bar-z x))))

(: foo/sym-or-num? (-> Any Boolean : (Prefab foo (U Symbol Number))))
(define (foo/sym-or-num? x)
  (and (foo? x) (or (symbol? (foo-x x))
                    (number? (foo-x x)))))

(: foo/sym-or-str? (-> Any Boolean : (Prefab foo (U Symbol String))))
(define (foo/sym-or-str? x)
  (and (foo? x) (or (symbol? (foo-x x))
                    (string? (foo-x x)))))

(: foo/num-or-str? (-> Any Boolean : (Prefab foo (U Number String))))
(define (foo/num-or-str? x)
  (and (foo? x) (or (number? (foo-x x))
                    (string? (foo-x x)))))

(: read-known-foo3 (-> Any Symbol))
(define (read-known-foo3 f)
  (if (and (foo/sym-or-num? f)
           (foo/sym-or-str? f)
           (foo/num-or-str? f))
      (+ 'dead "code")
      'other))

(: empty-intersection-check1 (-> Any Any))
(define (empty-intersection-check1 x)
  (cond
    [(and (foo? x) (foo*? x)) (+ 1 "2")]
    [else 42]))

(: empty-intersection-check2 (-> Any Any))
(define (empty-intersection-check2 x)
  (cond
    [(and (foo? x) (bar*? x)) (+ 1 "2")]
    [else 42]))

;; Mutable prefab structs

(struct baz ([x : String]) #:mutable #:prefab)
(define-struct baz* ([x : String]) #:mutable #:prefab)

(define a-baz (baz "baz"))
(set-baz-x! a-baz "baz2")
(baz-x a-baz)

(define a-baz* (make-baz* "baz"))
(set-baz*-x! a-baz* "baz2")
(baz*-x a-baz*)


(: set-some-baz-x! (All (A) (-> (Prefab (baz #(0)) A) A Void)))
(define (set-some-baz-x! b val)
  (set-baz-x! b val)) 


(: read-some-baz-x (All (A) (-> (Prefab (baz #(0)) A) A)))
(define (read-some-baz-x b)
  (baz-x b))


(: read-some-unknown-baz-x1 (-> (PrefabTop (baz #(0)) 1) Any))
(define (read-some-unknown-baz-x1 b)
  (baz-x b))


(: read-some-unknown-baz-x2 (-> Any Any))
(define (read-some-unknown-baz-x2 b)
  (if (baz? b)
      (baz-x b)
      42))


;; Polymorphic prefab structs

(struct (X) poly ([x : X]) #:prefab)
(define-struct (X) poly* ([x : X]) #:prefab)

(poly-x (poly "foo"))
(poly-x (poly 3))
(poly-x #s(poly "foo"))

(poly*-x (make-poly* "foo"))
(poly*-x (make-poly* 3))
(poly*-x #s(poly* "foo"))

;; Test match (indirectly tests unsafe-struct-ref)
(match (foo 'x) [(foo s) s])
(match (foo* 'x) [(foo* s) s])

(struct point ([x : Number] [y : Number])
  #:prefab)

;; although point prefabs with strings can be
;; created, the constructor `point` should enforce
;; the fields we asked it to enforce
(assert-typecheck-fail
 (point "1" "2")) 

;; the point predicate does not tell us
;; what values are in the point
(: maybe-point-x (-> Any Void))
(define (maybe-point-x p)
  (when (point? p)
    (assert-typecheck-fail (ann (point-x p) Number))
    (void)))


(struct initials ([first : Symbol] [last : Symbol])
  #:prefab
  #:mutable)


;; although initials prefabs with strings can be
;; created, the constructor `initials` should enforce
;; the fields we asked it to enforce
(assert-typecheck-fail
 (initials "1" "2"))


;; the initials predicate does not tell us
;; what values are in the field
(: maybe-initials-first (-> Any Symbol))
(define (maybe-initials-first i)
  (cond
    [(initials? i)
     (assert-typecheck-fail
      (initials-first i))
     'yay!]
    [else 'also-okay]))  

;; we cannot write to a mutable prefab when
;; we do not know what type it's fields have
;; (and the predicate does not tell us that)
(: maybe-set-initials-first! (-> Any Void))
(define (maybe-set-initials-first! i)
  (when (initials? i)
    (assert-typecheck-fail
     (set-initials-first! i (error "any-value")))))

;; should fail because the number of fields for initials is
;; incorrect (i.e. the initials we defined above has 2 fields
;; but this PrefabTop says its for initials with 3 fields)
(: set-some-initials-first! (-> (PrefabTop (initials #(0 1)) 3) Void))
(define (set-some-initials-first! i)
  (assert-typecheck-fail
   (set-initials-first! i (error "any-value"))))



(struct (A B) tuple ([fst : A] [snd : B]) #:prefab)

(define-predicate is-tuple? (tuple Any Any))
(define-predicate is-num-tuple? (tuple Number Number))

(define good-point (tuple 1 2))
(define bad-point1 (tuple "1" 2))
(define bad-point2 (tuple 1 "2"))

(unless (is-tuple? (if (zero? (random 1)) good-point "other value"))
  (error "is-tuple? broken!"))

(unless (is-num-tuple? (if (zero? (random 1)) good-point "other value"))
  (error "is-tuple? broken!"))

(when (is-num-tuple? (if (zero? (random 1)) bad-point1 good-point))
  (error "is-num-tuple? broken!"))

(when (is-num-tuple? (if (zero? (random 1)) bad-point2 good-point))
  (error "is-num-tuple? broken!"))

(struct (A B) unops ([fst : (-> A A)] [snd : (-> B B)]) #:prefab)

(assert-typecheck-fail
 (define-predicate is-unops (unops Any Any)))

(struct num-boxes ([fst : Number] [snd : Number]) #:prefab #:mutable)

(assert-typecheck-fail
 (define-predicate is-num-boxes num-boxes))