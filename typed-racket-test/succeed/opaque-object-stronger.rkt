#lang racket
(require typed-racket/utils/opaque-object)

;; --------------------------------------------------------------------------------------------------
;; test helpers

(define (test-stronger? ctc-stronger ctc-weaker)
  (unless (contract-stronger? ctc-stronger ctc-weaker)
    (error 'pr267
           "contract ~a is unexpectedly weaker than ~a"
           (contract-name ctc-stronger)
           (contract-name ctc-weaker))))

(define (test-not-stronger? ctc-stronger ctc-weaker)
  (when (contract-stronger? ctc-stronger ctc-weaker)
    (error 'pr267
           "contract ~a is unexpectedly stronger than ~a"
           (contract-name ctc-stronger)
           (contract-name ctc-weaker))))

;; --------------------------------------------------------------------------------------------------
;; stronger? tests

(let () ;; object/c-opaque with the same members
  (test-stronger?
    (object/c-opaque)
    (object/c-opaque))

  (test-stronger?
    (object/c-opaque
      (field (f1 integer?))
      (m1 (->m object? object?)))
    (object/c-opaque
      (field (f1 integer?))
      (m1 (->m object? object?))))

  (test-stronger?
    (object/c-opaque
      (m1 (->m any/c object? (-> integer? integer?))))
    (object/c-opaque
      (m1 (->m any/c object? (-> integer? integer?)))))
)

(let () ;; object/c-opaque with fewer members (unspecified = opaque)
  (test-stronger?
    (object/c-opaque)
    (object/c-opaque
      (field (x symbol?))))

  (test-stronger?
    (object/c-opaque
      (field (x symbol?)))
    (object/c-opaque
      (field (x symbol?))
      (y (->m none/c none/c))))

  (test-stronger?
    (object/c-opaque
      (f (->m void? any/c)))
    (object/c-opaque
      (f (->m void? any/c))
      (g (->m integer? integer? integer?))))

  (test-stronger?
    (object/c-opaque
      (field (a integer?))
      (c (-> real? real?)))
    (object/c-opaque
      (field (a integer?)
             (b integer?))
      (c (-> real? real?))
      (d (-> real? real?))))

)

(let () ;; object/c-opaque with stronger members

  (test-stronger?
    (object/c-opaque
      (m1 (->m any/c integer?)))
    (object/c-opaque
      (m1 (->m any/c any/c))))

  (test-stronger?
    (object/c-opaque
      (m1 (->m any/c any/c)))
    (object/c-opaque
      (m1 (->m integer? any/c))))

  (test-stronger?
    (object/c-opaque
      (m1 (->m any/c integer?))
      (m2 (->m any/c (listof boolean?))))
    (object/c-opaque
      (m1 (->m any/c integer?))
      (m2 (->m any/c (listof any/c)))))

  (test-stronger?
    (object/c-opaque
      (a (->m symbol?))
      (b (->m (between/c 2 3)))
      (c (->m any/c (listof (char-in #\A #\B)))))
    (object/c-opaque
      (a (->m symbol?))
      (b (->m (between/c 0 5)))
      (c (->m any/c (listof (char-in #\A #\Z))))))
)

(let () ;; vs. object/c
  (test-stronger?
    (object/c-opaque)
    (object/c
      (field (a boolean?))
      (b (->m string? any/c))))

  (test-stronger?
    (object/c-opaque
      (field (x any/c))
      (h (->m (-> boolean? boolean? boolean?) integer?)))
    (object/c))

  (test-stronger?
    (object/c-opaque
      (field (x integer?))
      (m1 (->m any/c any/c any/c)))
    (object/c
      (field (x integer?))
      (m1 (->m any/c any/c any/c))))

  (test-stronger?
    (object/c-opaque
      (m1 (->m any/c (</c 2))))
    (object/c
      (field (a real?) (b boolean?))
      (m1 (->m any/c (</c 10)))))
)

;; --------------------------------------------------------------------------------------------------
;; not-stronger? tests

(let () ;; fields must be the same for stronger?
  (test-not-stronger?
    (object/c-opaque
      (field (number (</c 999))))
    (object/c-opaque
      (field (number (</c 1)))))

  (test-not-stronger?
    (object/c-opaque
      (field (number (</c 1))))
    (object/c-opaque
      (field (number (</c 999)))))

  (test-not-stronger?
    (object/c-opaque
      (field (a symbol?)
             (b integer?)
             (c (-> any/c (listof zero?)))))
    (object/c-opaque
      (field (a symbol?)
             (b real?)
             (c (-> any/c (listof exact-nonnegative-integer?))))))
)

(let () ;; an object/c is never stronger than an object/c-opaque
  (test-not-stronger?
    (object/c-opaque
      (f (->m integer? integer?)))
    (object/c
      (f (->m any/c none/c))))

  (test-not-stronger?
    (object/c
      (f (->m any/c none/c)))
    (object/c-opaque
      (f (->m integer? integer?))))

  (test-not-stronger?
    (object/c
      (field (x integer?))
      (m1 (->m any/c any/c any/c)))
    (object/c-opaque
      (field (x real?))
      (m1 (->m any/c any/c any/c))))

)
