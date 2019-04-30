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
  #:property prop:evt 0

  #:property prop:custom-print-quotable 'always)

(struct foobar^ foo ([y : Number])
  #:property prop:custom-write
  (lambda ([self : foobar^] [p : Output-Port] [m : (U Boolean 1 0)]) : Void
          (displayln (+ (foobar^-y self) 20) p))

  #:property prop:evt (make-channel)

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
  (lambda ([self : poly-foo] [p : Output-Port] [m : (U Boolean 1 0)]) : Void
          (printf "~a : ~a" (poly-foo-x self) (poly-foo-y self))))

(struct (A B) poly-foo^ ([x : A] [y : B])
  #:property prop:custom-write
  (lambda (self p b)
          (printf "~a : ~a" (poly-foo^-x self) (poly-foo^-y self))))
