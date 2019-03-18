#lang typed/racket
(struct dummy ([x : Number]))

(struct foo-dummy dummy ([x : Number]) #:property prop:custom-write
  (lambda ([self : dummy] [p : Output-Port] [b : Boolean]) : Void
          (displayln (+ (dummy-x self) 20))))

(struct foo ([x : Number]) #:property prop:custom-write
  (lambda ([self : foo] [p : Output-Port] [b : Boolean]) : Void
          (displayln (+ (foo-x self) 20))))

(struct foobar foo ([y : Number]) #:property prop:custom-write
  (lambda ([self : foo] [p : Output-Port] [b : Boolean]) : Void
          (displayln (+ (foo-x self) 20) p)))

(struct foobar^ foo ([y : Number]) #:property prop:custom-write
  (lambda ([self : foobar^] [p : Output-Port] [b : Boolean]) : Void
          (displayln (+ (foobar^-y self) 20) p)))

(struct foobarabc foobar ([z : Number]) #:property prop:custom-write
  (lambda ([self : foo] [p : Output-Port] [b : Boolean]) : Void
          (displayln (+ (foo-x self) 20) p)))

(struct foobarabc^ foobar ([z : Number]) #:property prop:custom-write
  (lambda ([self : foobar] [p : Output-Port] [b : Boolean]) : Void
          (displayln (+ (foobar-y self) 20) p)))


(struct (A B) poly-foo ([x : A] [y : B])
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (lambda ([self : poly-foo] [p : Output-Port] [b : Boolean]) : Void
          (printf "~a : ~a" (poly-foo-x self) (poly-foo-y self))))

(struct (A B) poly-foo^ ([x : A] [y : B])
  #:property prop:custom-write
  (lambda (self p b)
          (printf "~a : ~a" (poly-foo^-x self) (poly-foo^-y self))))
