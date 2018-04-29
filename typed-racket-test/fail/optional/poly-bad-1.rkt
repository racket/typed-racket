#;
(exn-pred exn:fail:contract? #rx"car")

#lang typed/racket/base/optional

(require/typed racket/base
  (cdr (All (A) (U (Boxof A) A))))

(let ((v : (U (Boxof (Pairof String String)) (Pairof String String))
       (inst cdr (Pairof String String))))
  (if (box? v)
    #f
    (car v)))

