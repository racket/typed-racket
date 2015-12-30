#lang typed/racket/base

(require/typed
 net/url-structs
 [#:struct path/param
           ([path : (U String 'up 'same)]
            [param : (Listof String)])
           #:type-name Path/Param])

(ann (path/param "path" null) Path/Param)
