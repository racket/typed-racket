#lang typed/racket/base

(require typed/racket/unsafe)

(define-type Glob (U Path-String (Sequenceof Path-String)))

(unsafe-require/typed file/glob
  [glob (->* [Glob] [#:capture-dotfiles? Boolean] (Listof Path-String))]
  [in-glob (->* [Glob] [#:capture-dotfiles? Boolean] (Sequenceof Path-String))]
  [glob-match? (->* [Glob Path-String] [#:capture-dotfiles? Boolean] Boolean)]
  [glob-capture-dotfiles? (Parameterof Boolean)])

(provide glob
         in-glob
         glob-match?
         glob-capture-dotfiles?)
