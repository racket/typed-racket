#lang racket
(require (only-in typed-racket/base-env/base-env [org-map be:org-map])
         syntax/srcloc
         (only-in typed-racket/base-env/base-env-numeric [org-map ben:org-map]))

(for ([a (in-list (append be:org-map
                          ben:org-map))]
      #:unless
      ;; skip assert, because it is a macro that requires to be called with arguments
      (equal? (syntax-e (car a)) 'assert))
  (with-handlers ([exn? (lambda (e)
                          (eprintf "~a:~n the type for ~a contains error:~n ~a ~n" (srcloc->string (build-source-location (car a)))
                                   (syntax-e (car a))
                                   (exn-message e)))])
    ((car (cdr a)))))
