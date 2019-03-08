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
