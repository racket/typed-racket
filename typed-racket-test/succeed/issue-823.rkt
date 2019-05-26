#lang typed/racket/base

;; any-wrap/traverse should not loop on cyclic values
;; https://github.com/racket/typed-racket/issues/823

;; (2019-12-17) This test creates a cyclic value, sends it across a boundary,
;;  and looks for an error message. Ideally, though, a copy of the value
;;  should be able to cross

(module u racket/base
  (provide cyclic-val do-traverse)

  (define cyclic-val
    (let* ([ph (make-placeholder #f)]
           [x (cons 0 ph)])
      (placeholder-set! ph x)
      (make-reader-graph x)))

  (define (do-traverse x) #true))

(require/typed 'u
  (cyclic-val Any)
  (do-traverse (-> Any Any)))

(with-handlers ([exn:fail:contract?
                 (λ ([e : exn])
                   (unless (regexp-match? #rx"acyclic value" (exn-message e))
                     (error 'issue-823:bad-error-message)))])
  (do-traverse cyclic-val))
