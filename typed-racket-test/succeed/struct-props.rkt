#lang typed/racket
(struct dummy ([x : Number]))

(struct foo-dummy dummy ([x : Number])
  #:property prop:custom-write
  (lambda ([self : dummy] [p : Output-Port] [m : (U Boolean 1 0)]) : Void
          (displayln (+ (dummy-x self) 20))))

(struct foo ([x : Number])
  #:property prop:custom-write
  (lambda ([self : foo] [p : Output-Port] [m : (U Boolean 1 0)]) : Void
          (displayln (+ (foo-x self) 20)))

  #:property prop:evt
  (lambda ([self : foo]) : Void
          (display (+ (foo-x self) 20)))

  #:property prop:custom-print-quotable 'never)

(struct foobar foo ([y : Number])
  #:property prop:custom-write
  (lambda ([self : foo] [p : Output-Port] [m : (U Boolean 1 0)]) : Void
          (displayln (+ (foo-x self) 20) p))
  #:property prop:custom-print-quotable 'always)

(struct foobar^ foo ([y : Number])
  #:property prop:custom-write
  (lambda ([self : foobar^] [p : Output-Port] [m : (U Boolean 1 0)]) : Void
          (displayln (+ (foobar^-y self) 20) p))

  #:property prop:evt (ann (make-channel) (Evtof Any))

  #:property prop:custom-print-quotable 'self)

(struct foobarabc foobar ([z : Number])
  #:property prop:custom-write
  (lambda ([self : foo] [p : Output-Port] [m : (U Boolean 1 0)]) : Void
          (displayln (+ (foo-x self) 20) p))

  #:property prop:custom-print-quotable 'maybe)

(struct foobarabc^ foobar ([z : Number])
  #:property prop:custom-write
  (lambda ([self : foobar] [p : Output-Port] [m : (U Boolean 1 0)]) : Void
          (displayln (+ (foobar-y self) 20) p)))

;; test polymorphic structs
(struct (A B) poly-foo ([x : A] [y : B])
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (lambda ([self : (poly-foo A B)] [p : Output-Port] [m : (U Boolean 1 0)]) : Void
          (printf "~a : ~a" (poly-foo-x self) (poly-foo-y self))))

(struct (A B) poly-foo^ ([x : A] [y : B])
  #:property prop:custom-write
  (lambda (self p b)
          (printf "~a : ~a" (poly-foo^-x self) (poly-foo^-y self))))

;; test prop:convertible
(require file/convertible)
(struct conv-foo []
  #:property prop:convertible
  (λ ([self : conv-foo] [request : Symbol] [default : Any])
    (cond
      [(eqv? request 'text) default]
      ;[(eqv? request 'gif-bytes) default]
      [(eqv? request 'png-bytes+bounds) default]
      [else default])))


(require mzlib/pconvert-prop)
(struct pconvert-foo [[x : Number]]
  #:property prop:print-converter
  (lambda ([self : pconvert-foo] [f : (-> Any Any)])
          (f (pconvert-foo-x self))))


;; # test imp in prop:eqaul+hash
(struct equal-foo-boring ([x : Number]) #:mutable
  #:property prop:equal+hash
  (list
   (lambda #:∀ (B) ([self : equal-foo-boring] [other : equal-foo-boring] [conv : (-> Any Any Boolean)]) : Any
             10)
   (lambda #:∀ (A) ([self : equal-foo-boring] [conv : (-> Any Integer)]) : Integer
           10)
   (lambda #:∀ (A) ([self : equal-foo-boring] [conv : (-> Any Integer)]) : Integer
           10)))

(struct (T) equal-foo ([x : T]) #:mutable
  #:property prop:equal+hash
  (list
   (lambda #:∀ (B) ([self : (equal-foo T)] [other : (equal-foo B)] [conv : (-> Any Any Boolean)]) : Any
             10)
   (lambda #:∀ (A) ([self : (equal-foo A)] [conv : (-> Any Integer)]) : Integer
           10)
   (lambda #:∀ (A) ([self : (equal-foo A)] [conv : (-> Any Integer)]) : Integer
           10)))

(struct (T V) equal-foo-two ([x : T] [y : V]) #:mutable
  #:property prop:equal+hash
  (list
   (lambda #:∀ (A B) ([self : (equal-foo-two T V)] [other : (equal-foo-two A B)] [conv : (-> Any Any Boolean)]) : Any
             10)
   (lambda #:∀ (A B) ([self : (equal-foo-two A B)] [conv : (-> Any Integer)]) : Integer
           10)
   (lambda #:∀ (A B) ([self : (equal-foo-two A B)] [conv : (-> Any Integer)]) : Integer
           10)))

(struct a-struct-with-construct-id ([x : Number])
  #:constructor-name ACons
  #:property prop:custom-write
  (lambda ([self : a-struct-with-construct-id] [p : Output-Port] [m : (U Boolean 1 0)]) : Void
          (displayln (+ (a-struct-with-construct-id-x self) 20))))
