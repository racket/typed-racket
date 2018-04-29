#lang typed/racket/base

;; Fragment of the GTP benchmark take5
;;
;; 2022-07-12 raises an internal error on the `transient-pr` branch
;;  when typechecking an occurrence of `internal%`,
;;  specifically when calling `setter->type`

(require
  typed/racket/class)

(define-type Name Natural)
(define-type Face Natural)
(define-type Bulls Natural)

(struct card (
 [face : Face]
 [bulls : Bulls])
#:transparent)

(define-type Card card)

(define-type Player%
  (Class
    (init-field (n Name))
    (field [my-cards [Listof Card]])))

(define-type Player (Instance Player%))

(: player% Player%)
(define player%
  (class object%
    (init-field n)
    (field [my-cards '()])
    (super-new)))

(define-type Internal%
  (Class
    #:implements Player%
    (init-field [player Player])
    (field [my-bulls Natural])))

(define-type Internal (Instance Internal%))

(define-type Dealer%
  (Class
    (init-field (players (Listof Player)))
    (field
     (internal% Internal%)
     (internals (Listof Internal)))))

(define-type Dealer (Instance Dealer%))

(define dealer% : Dealer%
  (class object%
    (init-field
     (players : (Listof Player)))

    (super-new)

    (field
     [internal% : Internal%
      (class player%
        (init-field player)
        (super-new [n 0])
        (field [my-bulls 0]))]
     [internals (for/list : (Listof Internal)
                          ([p : Player (in-list (get-field players this))])
                  (new internal% [player p]))])
    ))

