#lang typed/racket/base

(require/typed/provide
 net/url-structs
 [#:struct path/param
           ([path : (U String 'up 'same)]
            [param : (Listof String)])]
 [#:struct url
           ([scheme : (Option String)]
            [user : (Option String)]
            [host : (Option String)]
            [port : (Option Natural)]
            [path-absolute? : Boolean]
            [path : (Listof path/param)]
            [query : (Listof (Pairof Symbol (Option String)))]
            [fragment : (Option String)])])

(define-type Path/Param path/param)
(define-type URL url)
(provide Path/Param URL)
