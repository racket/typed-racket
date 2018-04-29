#lang typed/racket/optional

;; Plot error:
;;
;;    /Users/ben/code/racket/fork/racket/share/pkgs/plot-lib/plot/private/plot3d/isosurface.rkt:130:23: Type Checker: No function domains matched in function application:
;;    Domains: Any (Sequenceof (Parameterof Plot-Pen-Style) Any)
;;             Any (Sequenceof (Parameterof Plot-Pen-Style))
;;             Any Integer
;;    Arguments: Any (Sequenceof Plot-Pen-Style)
;;      in: (in-cycle* line-styles)
;;
;; Try to reproduce ... failure so far (2019-08-22), but
;; - error goes away if remove `rts` provides from the #%type-decl
;; - error STAYS if remove only the unhygienic lookups
;; so the problem is related to the new exports for the "contract + type" hack,
;;  and maybe by improving that we'll get a better fix
;;
;; Ok now we are simplified; the issue is a defined variable vs a for-loop variable

(: in-cycle* (All (A) (-> (Sequenceof A) (Sequenceof A))))
(define (in-cycle* s)
  (define n (sequence-length s))
  (if (zero? n) empty-sequence (in-cycle s)))

(define xxx : String "hello")

(: f (-> (Listof Symbol) Void))
(define (f line-styles)
  (for ([xxx  (in-cycle* line-styles)])
    (void)))
