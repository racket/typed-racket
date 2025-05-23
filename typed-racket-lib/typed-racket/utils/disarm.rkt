#lang racket/base

(provide (protect-out disarm*))

;; Typed Racket runs after macro expansion, and it must be priviledged,
;; so it can just disarm all taints (and arm everything afterward).

(define (disarm* stx)
  (let loop ([v stx])
    (cond
     [(syntax? v)
      (let* ([stx v]
             [r (loop (syntax-e stx))])
        (if (eq? r (syntax-e stx))
            stx
            (datum->syntax stx r stx stx)))]
     [(pair? v) (define a (loop (car v)))
                (define d (loop (cdr v)))
                (if (and (eq? a (car v)) (eq? d (cdr v)))
                    v
                    (cons a d))]
     [else v])))

(define orig-insp (variable-reference->module-declaration-inspector
                   (#%variable-reference)))


