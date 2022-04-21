#lang typed/racket/base

(define ch ((inst make-channel Number)))


(struct aaa0 ((evt : (Evtof Number)))
  #:property prop:evt (struct-field-index evt))

(thread (lambda ()
          (channel-put ch 10)))

(ann (sync (aaa0 ch)) Number)


(struct aaa1 ([evt : (Evtof Number)])
  #:property prop:evt 0)

(thread (lambda ()
          (channel-put ch 10)))

(ann (sync (aaa1 ch)) Number)

(struct aaa2 ([evt : (Evtof Number)])
  #:property prop:evt (lambda ([self : aaa2]) : (Evtof Number)
                        (aaa2-evt self)))

(thread (lambda ()
          (channel-put ch 10)))
(ann (sync (aaa2 ch)) Number)


(define ch2 ((inst make-channel String)))
(struct aaa3 ()
  #:property prop:evt (ann ch2 (Evtof String)))

(thread (lambda ()
          (channel-put ch2 "10")))

(ann (sync (aaa3)) String)
