#lang typed-racket/minimal

(require typed/racket/base/shallow racket/require
         (subtract-in racket typed/racket/base/shallow racket/contract
                      typed/racket/class
                      typed/racket/unit)
         typed/racket/class
         typed/racket/unit
	 (for-syntax racket/base))
(provide (all-from-out racket
                       typed/racket/base/shallow
                       typed/racket/class
                       typed/racket/unit)
	 (for-syntax (all-from-out racket/base))
         class)
