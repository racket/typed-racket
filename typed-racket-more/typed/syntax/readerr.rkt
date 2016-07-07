#lang typed/racket/base

;; Since typed racket can generate contracts for these, this doesn't
;; need to use unsafe-require/typed.
(require/typed/provide syntax/readerr
                       [raise-read-error
                        (-> String
                            Any
                            (U Positive-Integer False)
                            (U Natural False)
                            (U Positive-Integer False)
                            (U Natural False)
                            [#:extra-srclocs (Listof srcloc)]
                            Nothing)]
                       [raise-read-eof-error
                        (-> String
                            Any
                            (U Positive-Integer False)
                            (U Natural False)
                            (U Positive-Integer False)
                            (U Natural False)
                            Nothing)]
                       )
