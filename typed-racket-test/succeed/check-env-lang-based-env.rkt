#lang typed/racket
(require (only-in racket/extflonum floating-point-bytes->extfl extfl->floating-point-bytes))
(require racket/unsafe/ops)
(require racket/symbol)
(require racket/keyword)
(require compatibility/mlist
         racket/unsafe/undefined
         racket/private/stx
         racket/file
         '#%paramz
         racket/logging
         (only-in racket/private/pre-base new-apply-proc)
         (only-in '#%kernel [apply kernel:apply] [reverse kernel:reverse])
         (only-in racket/private/pre-base new-apply-proc)
         (only-in file/convertible prop:convertible convertible?)
         (only-in mzlib/pconvert-prop prop:print-converter prop:print-convert-constructor-name print-converter?)
         (only-in mzscheme make-namespace)
         (only-in racket/match/runtime match:error matchable? match-equality-test syntax-srclocs)
         racket/hash)

(require (for-syntax (only-in typed-racket/base-env/base-env [org-map be:org-map])
                     (only-in typed-racket/base-env/base-env-numeric [org-map ben:org-map])))

(define-syntax (run stx)
  (with-syntax ([(a ...) (for/list ([a (in-list (append be:org-map
                                                        ben:org-map))]
                                    #:unless
                                    ;; skip assert, because it is a macro that requires to be called with arguments
                                    (equal? (syntax-e (car a)) 'assert))
                           (car a))])
    #'(begin a ...)))
(run)
