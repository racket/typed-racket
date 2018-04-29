#lang racket/load
(require typed/racket/shallow)
(:print-type +)
(:print-type (values 2 3 4))
(:print-type (error 'ManyTypes))
