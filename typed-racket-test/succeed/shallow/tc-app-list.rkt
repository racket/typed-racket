#lang typed/racket/base/shallow

;; Test special-cases in tc-app-list

(list* '(1 2 3))

(ann (reverse '(1 2 3)) (Listof Natural))
(ann (reverse '(1 2 3)) (List Natural Natural Natural))
(reverse '(1 2 3))
(lambda ((xs : (Listof Natural)))
  (reverse xs))
(lambda ((xs : (Pairof Integer (Listof Integer))))
  (reverse xs))

(: main (-> String Void))
(define (main filename)
  (define raw (with-input-from-file filename read))
  (if (list? raw)
    (void (ann (reverse raw) (Listof Any)))
    (error "bad input")))
