#lang typed/racket/base

(define ch ((inst make-channel Number)))


(struct aaa0 ((evt : (Channelof Number)))
  #:property prop:evt (struct-field-index evt))

(thread (lambda ()
          (channel-put ch 10)))

(ann (sync (aaa0 ch)) Number)


(struct aaa1 ([evt : (Evtof Number)])
  #:property prop:evt 0)

(thread (lambda ()
          (channel-put ch 10)))

(ann (sync (aaa1 ch)) Number)

(struct aaa2 ([evt : (Channelof Number)])
  #:property prop:evt (lambda ([self : aaa2]) : (Channelof Number)
                        (aaa2-evt self)))

(thread (lambda ()
          (channel-put ch 10)))
(ann (sync (aaa2 ch)) Number)


(define ch2 ((inst make-channel String)))
(struct aaa3 ()
  #:property prop:evt (ann ch2 (Channelof String)))

(thread (lambda ()
          (channel-put ch2 "10")))

(ann (sync (aaa3)) String)


(struct aaa4 ([b : String]) #:property prop:evt (lambda (self) : Number
                                                  (string-length (aaa4-b self))))

(ann (sync (aaa4 "hello")) aaa4)


(struct (A) poly-aaa ([b : A]) #:property prop:evt (lambda (self) : A
                                                     (poly-aaa-b self)))

(ann (sync (poly-aaa "hello")) (poly-aaa String))

(thread (lambda ()
          (channel-put ch 10)))
(ann (sync (poly-aaa ch))  Number)


(struct aaa-never ([b : String]) #:property prop:evt (struct-field-index b))

((lambda ([a : (Evtof Nothing)]) : String
   "42")
 (aaa-never "84"))
