#lang typed/racket/base

(provide with-module-reading-parameterization check-module-form)

(require typed/racket/unsafe)

(unsafe-require/typed syntax/modread
                      [with-module-reading-parameterization
                       (All (A) (-> (-> A) A))]
                      [check-module-form
                       ;; check-module-form is already protected with the contract:
                       ;; (-> (or/c syntax? eof-object?)
                       ;;     (or/c symbol? list?)
                       ;;     (or/c string? path? false/c)
                       ;;     any) ; This is any because the contract doesn't need
                       ;;          ; to enforce that it's (or/c syntax? false/c).
                       ;;                             ; From the documentation:
                       (-> (U (Syntaxof Any) EOF)     ; (or/c syntax? eof-object?)
                           Symbol                     ; symbol?
                           (U String False)           ; (or/c string? false/c)
                           (U (Syntaxof Any) False))] ; (or/c syntax? false/c)
                      )
