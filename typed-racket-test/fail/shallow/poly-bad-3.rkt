#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang typed/racket/base/shallow

;; Expected: shape-check error
;;
;; With a similar program, we can end up passing a bad input to `cdr`,
;;  which means that Shallow functions need to be careful to protect themselves
;;  (untyped & typed need to take care too, of course).

(require/typed racket/base
  (cdr (All (A) (U (Boxof A) (Pairof A A) (-> (Pairof String String) Symbol)))))

(let ((v : (U (Boxof String) (Pairof String String) (-> (Pairof String String) Symbol))
       (inst cdr String)))
  (if (box? v)
    #f
    (if (pair? v)
      #f
      (v (cons "a" "aaa")))))

