#lang typed/racket/base/optional

(require/typed racket/base
  (cdr (All (A) (U (Boxof A) (Pairof A A) (-> (Pairof String String) Symbol)))))

(let ((v : (U (Boxof String) (Pairof String String) (-> (Pairof String String) Symbol))
       (inst cdr String)))
  (if (box? v)
    #f
    (if (pair? v)
      #f
      (v (cons "a" "aaa")))))

