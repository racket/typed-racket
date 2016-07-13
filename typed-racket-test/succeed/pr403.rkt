#lang typed/racket

;; https://github.com/racket/typed-racket/issues/403
;; Ran forever, fix involved making overlap
;; keep a "current-seen" list to not
;; keep resolving types it had already seen before.

(define-type (List3-Maybe Start Mid End)
  (Listof* Start
           (U Null
              (Pairof Mid (Listof End)))))

(define-type (List3 Start Mid End)
  (Listof* Start
           (Pairof Mid (Listof End))))

(define-type (Listof* Start End)
  (Rec R (U (Pairof Start R)
            End)))

(: replace-first (∀ (A B1 B2 C D)
                    (case→
                     (→ C
                        (Listof (U A B1))
                        (→ (U A B1) Any : #:+ B1 #:- (! B1))
                        (List3-Maybe A C (U A B1)))
                     (→ C
                        (Listof* A (U Null (Pairof B2 D)))
                        (→ (U A B2) Any : #:+ (! A) ;; ∴ (and (! A) B2)
                           #:- (! B2))
                        (Listof* A (U Null (Pairof C D))))
                     (→ C
                        (Listof* A (Pairof B2 D))
                        (→ (U A B2) Any : #:+ (! A) ;; ∴ (and (! A) B2)
                           #:- (! B2))
                        (Listof* A (Pairof C D)))
                     (→ C
                        (Listof A)
                        (→ (U A B1) Any)
                        (List3-Maybe A C (U A B1)))
                     (→ A
                        C
                        (Listof A)
                        (List3-Maybe A C (U A B1)))
                     (→ A
                        C
                        (Listof A)
                        (→ A (U A B1) Any)
                        (List3-Maybe A C (U A B1))))))
(define (replace-first a1 a2 a3 [a4 eq?])
  (if (list? a3)
      (replace-first a2 a3 (λ ([x : (U A B1)]) (a4 a1 x)))
      (let ([to a1]
            [pred? a3])
        (let rec ([l a2])
          (if (null? l)
              '()
              (if (pred? (car l))
                  (cons to (cdr l))
                  (cons (car l)
                        (rec (cdr l)))))))))
