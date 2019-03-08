#lang typed/racket
(struct foo^ ([x : Number]) #:property prop:custom-write 10)
