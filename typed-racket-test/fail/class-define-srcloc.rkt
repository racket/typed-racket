#;
(exn-pred exn:fail:contract? #rx"division by zero" #rx"#.struct:srcloc #<path[^>]+class-define-srcloc.rkt> 22 4")

#lang typed/racket

(require/typed racket/base
               [#:struct exn
                 ([message : String]
                  [continuation-marks : Continuation-Mark-Set])]
               [#:struct (exn:fail exn) ()]
               [#:struct (exn:fail:contract exn:fail) ()
                 #:type-name Exn-Fail-Contract]
               [raise (-> Exn-Fail-Contract Number)])

;; https://github.com/racket/typed-racket/issues/1093
(define-type C%
  (Class (div (-> Number Number))))

(: c% C%)
(define c%
  (class object%
    (define/public (div n) (+ 1 (/ 1 n)))
    (super-new)))

(with-handlers ([exn:fail:contract?
                 (lambda ([e : Exn-Fail-Contract])
                   (raise
                    (exn:fail:contract
                     (format "~a\n  context data: ~s"
                             (exn-message e)
                             (continuation-mark-set->context
                              (exn-continuation-marks e)))
                     (exn-continuation-marks e))))])
  (* 2 (send (new c%) div 0)))
