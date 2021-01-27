#lang typed/racket

(define filename (if (eq? (system-type 'os) 'windows)
                     "C:\\windows\\system.ini"
                     "/dev/null"))
((values file->string) filename #:mode 'binary)
(file->string filename #:mode 'text)

file->value file->bytes

(file->lines #:mode 'text #:line-mode 'linefeed filename)
