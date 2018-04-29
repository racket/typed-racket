#lang racket/base

;; test evaluators, as used in Scribble docs

(require scribble/example)

(for ((the-eval (in-list (list
                           (make-base-eval #:lang 'typed/racket)
                           (make-base-eval '(require typed/racket))))))
  ;; define-predicate : non-function polymorphic type
  ;; cast : bad list
  (examples
    #:label #f #:eval the-eval
    (eval:error (define-predicate p? (All (A) (Listof A))))
    (require/typed racket/base
      [object-name (-> (U (Listof Real) Regexp)
                       (U #f String Bytes Symbol))])
    (object-name #rx"a regexp")
    (eval:error (object-name (cast '(A B C) (Listof Real))))))

(for ((the-eval (in-list (list
                           (make-base-eval #:lang 'typed/racket/shallow)
                           (make-base-eval '(require typed/racket/shallow))))))
  ;; object-name returns #f (shape-check error)
  (examples
    #:label #f #:eval the-eval
    (eval:error (define-predicate p? (All (A) (Listof A))))
    (require/typed racket/base
      [object-name (-> (U (Listof Real) Regexp)
                       (U String Bytes Symbol))])
    (object-name #rx"a regexp")
    (eval:error (object-name (cast '(A B C) (Listof Real))))))

(for ((the-eval (in-list (list
                           (make-base-eval #:lang 'typed/racket/optional)
                           (make-base-eval '(require typed/racket/optional))))))
  (examples
    #:label #f #:eval the-eval
    (eval:error (define-predicate p? (All (A) (Listof A))))
    (require/typed racket/base
      [object-name (-> (U (Listof Real) Regexp)
                       (U String Bytes Symbol))])
    (object-name #rx"a regexp")
    (object-name (cast '(A B C) (Listof Real)))))

