#lang typed/racket

(define filename (collection-file-path "simple-kw-app.rkt"
                                       "typed-racket-test"
                                       "succeed"))
((values file->string) filename #:mode 'binary)
(file->string filename #:mode 'text)

file->value file->bytes

(file->lines #:mode 'text #:line-mode 'linefeed filename)
