#lang typed/racket/base

(require/typed/provide
 racket/os
 [gethostname (-> String)]
 [getpid (-> Integer)])
