#lang typed/racket

(require racket/symbol
         racket/keyword)

(string-append-immutable (symbol->immutable-string 'a)
                         " "
                         (keyword->immutable-string '#:b))

