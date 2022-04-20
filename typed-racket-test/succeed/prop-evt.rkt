#lang typed/racket/base


(struct aaa0 ((evt : (Evtof Number)))
  #:property prop:evt (struct-field-index evt))

(ann (sync (aaa0 (make-channel))) Number)


(struct aaa1 ([evt : (Evtof Number)])
  #:property prop:evt 0)

(ann (sync (aaa1 (make-channel))) Number)

(struct aaa2 ([evt : (Evtof Number)])
  #:property prop:evt (lambda ([self : aaa2]) : (Evtof Number)
                        (aaa2-evt self)))

(ann (sync (aaa2 (make-channel))) Number)

(struct aaa3 ([evt : (Evtof String)])
  #:property prop:evt (ann (make-channel) (Evtof String)))

(ann (sync (aaa3 (make-channel))) String)
