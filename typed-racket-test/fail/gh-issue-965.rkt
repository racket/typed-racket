#lang typed/racket/base

(require racket/sequence)

(define (fun [vs : (U (Listof Real)
                      (Listof (Listof Real)))])

  (for/fold ([a : (Option (Listof (Listof Real))) '()])
            ([s vs])
    (define l (sequence->list s))
    (cond
      [(and (list? l) (= (length l) 2) (sequence? (car l)))
       (cons (cons (sequence->list (car l))
                   (sequence->list (cadr l)))
             a)]
      [else #f])))

