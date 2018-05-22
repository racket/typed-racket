#lang typed/racket

;; Test prefab struct declarations


(provide (all-defined-out))

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
      (initials-first i)
      #:result 'default)]
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


(struct num-num ([n1 : Number] [n2 : Number]) #:prefab)
(struct num-num-str-str-str num-num ([s1 : String] [s2 : String] [s3 : String]) #:prefab)


(num-num 1 2)
(assert-typecheck-fail
 (num-num 1 "2"))
(assert-typecheck-fail
 (num-num "1" 2))
(num-num-str-str-str 1 2 "1" "2" "3")
(assert-typecheck-fail
 (num-num-str-str-str 1 2 1 "2" "3"))
(assert-typecheck-fail
 (num-num-str-str-str 1 2 "1" 2 "3"))
(assert-typecheck-fail
 (num-num-str-str-str 1 2 "1" "2" 3))
(unless (equal? 1 (ann (num-num-n1 (num-num-str-str-str 1 2 "3" "4" "5")) Number))
  (error "oh no! didn't get 1!"))
(unless (equal? 1 (ann (num-num-n1 #s((num-num-str-str-str num-num 2) 1 2 "3" "4" "5")) Number))
  (error "oh no! didn't get 1!"))
(unless (equal? 2 (ann (num-num-n2 #s((num-num-str-str-str num-num 2) 1 2 "3" "4" "5")) Number))
  (error "oh no! didn't get 2!"))
(unless (equal? 2 (ann (num-num-n2 (num-num-str-str-str 1 2 "3" "4" "5")) Number))
  (error "oh no! didn't get 2!"))
(unless (equal? "3" (ann (num-num-str-str-str-s1 (num-num-str-str-str 1 2 "3" "4" "5")) String))
  (error "oh no! didn't get \"3\""))
(unless (equal? "3" (ann (num-num-str-str-str-s1 #s((num-num-str-str-str num-num 2) 1 2 "3" "4" "5")) String))
  (error "oh no! didn't get \"3\""))
(unless (equal? "4" (ann (num-num-str-str-str-s2 (num-num-str-str-str 1 2 "3" "4" "5")) String))
  (error "oh no! didn't get \"4\""))
(unless (equal? "4" (ann (num-num-str-str-str-s2 #s((num-num-str-str-str num-num 2) 1 2 "3" "4" "5")) String))
  (error "oh no! didn't get \"4\""))
(unless (equal? "5" (ann (num-num-str-str-str-s3 (num-num-str-str-str 1 2 "3" "4" "5")) String))
  (error "oh no! didn't get \"5\""))
(unless (equal? "5" (ann (num-num-str-str-str-s3 #s((num-num-str-str-str num-num 2) 1 2 "3" "4" "5")) String))
  (error "oh no! didn't get \"5\""))



(unless (equal?
         (let ([val : (PrefabTop (num-num-str-str-str num-num 2) 5)
                    #s((num-num-str-str-str num-num 2) "one" "two" 'three 'four 'five)])
           (ann (cond
                  [(not (num-num? val)) (+ "dead" 'code)]
                  [(not (num-num-str-str-str? val)) (+ "dead" 'code)]
                  [(string? (num-num-n1 val)) (num-num-n1 val)]  
                  [(string? (num-num-n2 val)) (num-num-n2 val)]
                  [(symbol? (num-num-str-str-str-s1 val)) (symbol->string (num-num-str-str-str-s1 val))]
                  [(symbol? (num-num-str-str-str-s2 val)) (symbol->string (num-num-str-str-str-s2 val))]
                  [(symbol? (num-num-str-str-str-s3 val)) (symbol->string (num-num-str-str-str-s3 val))]
                  [else "missed"])
                String))
         "one")
  (error "accessors broken num-num-str-str-str 1")) 

(unless (equal?
         (let ([val : Any #s((num-num-str-str-str num-num 2) "one" "two" 'three 'four 'five)])
           (ann (cond
                  [(not (num-num-str-str-str? val)) "nope"]
                  [(string? (num-num-n1 val)) (num-num-n1 val)] 
                  [(string? (num-num-n2 val)) (num-num-n2 val)]
                  [(symbol? (num-num-str-str-str-s1 val)) (symbol->string (num-num-str-str-str-s1 val))]
                  [(symbol? (num-num-str-str-str-s2 val)) (symbol->string (num-num-str-str-str-s2 val))]
                  [(symbol? (num-num-str-str-str-s3 val)) (symbol->string (num-num-str-str-str-s3 val))]
                  [else "missed"])
                String))
         "one")
  (error "accessors broken num-num-str-str-str 2"))
 
(unless (equal?
         (match (ann #s((num-num-str-str-str num-num 2) "one" "two" three four five) Any)
           [(num-num-str-str-str fld1 fld2 fld3 fld4 fld5)
            (list fld1 fld2 fld3 fld4 fld5)])
         (list "one" "two" 'three 'four 'five))
  (error "prefab pattern matching broken! num-num-str-str-str"))



(struct sym-sym ([n1 : Symbol] [n2 : Symbol]) #:prefab #:mutable)
(struct sym-sym-str-str-str sym-sym ([s1 : String] [s2 : String] [s3 : String]) #:prefab)


(sym-sym 'one 'two)
(sym-sym-str-str-str 'one 'two "1" "2" "3")
(unless (equal? 'one (ann (sym-sym-n1 (sym-sym-str-str-str 'one 'two "3" "4" "5")) Symbol))
  (error "oh no! didn't get 'one!"))
(unless (equal? 'two (ann (sym-sym-n2 (sym-sym-str-str-str 'one 'two "3" "4" "5")) Symbol))
  (error "oh no! didn't get 'two!"))
(unless (equal? "3" (ann (sym-sym-str-str-str-s1 (sym-sym-str-str-str 'one 'two "3" "4" "5")) String))
  (error "oh no! didn't get \"3\""))
(unless (equal? "4" (ann (sym-sym-str-str-str-s2 (sym-sym-str-str-str 'one 'two "3" "4" "5")) String))
  (error "oh no! didn't get \"4\""))
(unless (equal? "5" (ann (sym-sym-str-str-str-s3 (sym-sym-str-str-str 'one 'two "3" "4" "5")) String))
  (error "oh no! didn't get \"5\""))

(unless
    (equal?
     (let ([val : Any (read (open-input-string "#s((sym-sym-str-str-str sym-sym 2 #(0 1)) \"one\" \"two\" 'three 'four 'five)"))])
       (ann (cond
              [(not (sym-sym-str-str-str? val)) "nope"]
              [else
               (let ([fld1 (sym-sym-n1 val)]
                     [fld2 (sym-sym-n2 val)]
                     [fld3 (sym-sym-str-str-str-s1 val)]
                     [fld4 (sym-sym-str-str-str-s2 val)]
                     [fld5 (sym-sym-str-str-str-s3 val)])
                 (cond
                   [(string? fld1) fld1]
                   [(string? fld2) fld2]
                   [(symbol? fld3) (symbol->string fld3)]
                   [(symbol? fld4) (symbol->string fld4)]
                   [(symbol? fld5) (symbol->string fld5)]
                   [else "missed"]))])
            String))
     "one")
  (error "accessors broken sym-sym-str-str-str"))

(match (ann (read (open-input-string "#s((sym-sym-str-str-str sym-sym 2 #(0 1)) \"one\" \"two\" three four five)"))
            Any)
  [(sym-sym-str-str-str fld1 fld2 fld3 fld4 fld5)
   (unless (equal? (list fld1 fld2 fld3 fld4 fld5)
                   (list "one" "two" 'three 'four 'five))
     (error 'sym-sym-str-str-str "prefab pattern matching broken! got: ~a\n"
            (list fld1 fld2 fld3 fld4 fld5)))]
  [val (error "prefab pattern matching broken! sym-sym-str-str-str did not match ~a\n"
              val)])


(struct num-num-sym-sym-sym num-num ([s1 : Symbol] [s2 : Symbol] [s3 : Symbol]) #:prefab #:mutable)

(num-num-sym-sym-sym 1 2 'three 'four 'five)
(unless (equal? 1 (ann (num-num-n1 (num-num-sym-sym-sym 1 2 'three 'four 'five)) Number))
  (error "oh no! didn't get 1e!"))
(unless (equal? 2 (ann (num-num-n2 (num-num-sym-sym-sym 1 2 'three 'four 'five)) Number))
  (error "oh no! didn't get 2!"))
(unless (equal? 'three (ann (num-num-sym-sym-sym-s1 (num-num-sym-sym-sym 1 2 'three 'four 'five)) Symbol))
  (error "oh no! didn't get \"3\""))
(unless (equal? 'four (ann (num-num-sym-sym-sym-s2 (num-num-sym-sym-sym 1 2 'three 'four 'five)) Symbol))
  (error "oh no! didn't get \"4\""))
(unless (equal? 'five (ann (num-num-sym-sym-sym-s3 (num-num-sym-sym-sym 1 2 'three 'four 'five)) Symbol))
  (error "oh no! didn't get \"5\""))


(unless
    (equal?
     (let ([val : Any (read (open-input-string "#s((num-num-sym-sym-sym #(0 1 2) num-num 2) \"one\" \"two\" 3 4 5)"))])
       (ann (cond
              [(not (num-num-sym-sym-sym? val)) "nope"]
              [else
               (let ([fld1 (num-num-n1 val)] 
                     [fld2 (num-num-n2 val)]
                     [fld3 (num-num-sym-sym-sym-s1 val)]
                     [fld4 (num-num-sym-sym-sym-s2 val)]
                     [fld5 (num-num-sym-sym-sym-s3 val)])
                 (cond
                   [(string? fld1) fld1]
                   [(string? fld2) fld2]
                   [(number? fld3) (number->string fld3)]
                   [(number? fld4) (number->string fld4)]
                   [(number? fld5) (number->string fld5)]
                   [else "missed"]))])
            String))
     "one")
  (error "accessors broken num-num-sym-sym-sym"))

(struct str-str ([s1 : String] [s2 : String]) #:prefab #:mutable)
(struct str-str-num-num-num str-str ([n1 : Number] [n2 : Number] [n3 : Number]) #:prefab #:mutable)


(unless
    (equal?
     (let ([val : Any (read (open-input-string
                             "#s((str-str-num-num-num #(0 1 2) str-str 2 #(0 1)) one two three four five)"))])
       (ann (cond
              [(not (str-str-num-num-num? val)) "nope"]
              [else
               (let ([fld1 (str-str-s1 val)]
                     [fld2 (str-str-s2 val)]
                     [fld3 (str-str-num-num-num-n1 val)]
                     [fld4 (str-str-num-num-num-n2 val)]
                     [fld5 (str-str-num-num-num-n3 val)])
                 (cond
                   [(string? fld1) fld1]
                   [(string? fld2) fld2]
                   [(number? fld3) (number->string fld3)]
                   [(number? fld4) (number->string fld4)]
                   [(number? fld5) (number->string fld5)]
                   [else "missed"]))])
            String))
     "missed")
  (error "accessors broken num-num-sym-sym-sym"))

(unless (equal?
         (match (ann (read (open-input-string
                             "#s((str-str-num-num-num #(0 1 2) str-str 2 #(0 1)) one two three four five)"))
                     Any)
           [(str-str-num-num-num fld1 fld2 fld3 fld4 fld5)
            (list fld1 fld2 fld3 fld4 fld5)])
         (list 'one 'two 'three 'four 'five))
  (error "prefab pattern matching broken! str-str-num-num-num"))


(struct vec ([x : Float] [y : Float]))


(struct par-vec ([x : Float] [y : Float]) #:prefab)


(: partial-dec (-> Any vec))
(define (partial-dec x)
  (cond
    [(and (par-vec? x)
          (flonum? (par-vec-x x))
          (flonum? (par-vec-y x)))
     (vec (par-vec-x x) (par-vec-y x))]
    [else (error "invalid encoding")]))
