#lang racket/base

(require "test-utils.rkt"
         rackunit racket/format
         (rep prop-rep)
         (types abbrev prop-ops)
         (logic ineq)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)


;; binding these means props that mention #'x, #'y, and #'z
;; will be referring to identifiers w/ variable bindings
;; (and props about things w/o variable bindings are erased
;; since top level ids are mutable)
(define q 42)
(define r 42)
(define t 42)
(define x 42)
(define y 42)
(define z 42)



(define tests
  (let ([q (-id-path #'q)]
        [r (-id-path #'r)]
        [t (-id-path #'t)]
        [x (-id-path #'x)]
        [y (-id-path #'y)]
        [z (-id-path #'z)])
    (test-suite
     "LExps and Inequalities"
     (test-suite
      "LExp Basics"
      ;; some sanity checks on the construction of LExps
      ;; and basic properties about them
      (check-equal? (-lexp (list 1 x))
                    (-lexp x))
      (check-equal? (-lexp (list 1 x))
                    (-lexp 0 x))
      (check-equal? (-lexp 0
                           (list 1 x)
                           (list 1 x))
                    (-lexp 0 x x))
      (check-equal? (-lexp 0
                           (list 1 x)
                           (list 1 x))
                    (-lexp (list 2 x)))
      (check-equal? (constant-LExp? (-lexp 0
                                           (list 1 x)
                                           (list 1 x)))
                    #f)
      (check-equal? (constant-LExp? (-lexp 0
                                           (list 1 x)
                                           (list -1 x)))
                    0)
      (check-equal? (constant-LExp? (-lexp 42))
                    42)
      (check-equal? (constant-LExp? (-lexp -42))
                    -42))

     (test-suite
      "Leq Constructor Simplifications"
      ;; do obviously valid leqs get erased? do
      ;; others get turned into actual LeqProps, etc
      (check-equal? (-leq (-lexp 1) (-lexp 2)) -tt)
      (check-equal? (-leq (-lexp 2) (-lexp 1)) -ff)
      (check-equal? (-leq x
                          x)
                    -tt)
      (check-true (LeqProp? (-leq x
                                  (-id-path #'y))))
      (check-true (LeqProp? (-leq (-lexp (list 2 x))
                                  x)))
      (check-true (LeqProp? (-leq (-lexp 42 x)
                                  x)))
      (check-true (LeqProp? (-leq x
                                  (-lexp (list 2 x)))))
      (check-true (LeqProp? (-leq x
                                  (-lexp 42 x))))
      (check-true (LeqProp? (-leq (-lexp (list 2 x))
                                  x)))
      (check-true (LeqProp? (negate-prop
                             (-leq (-lexp (list 2 x))
                                   x))))
      (check-equal? (negate-prop
                     (-leq (-lexp (list 2 x))
                           x))
                    (-leq (-lexp 1 x)
                          (-lexp (list 2 x)))))
     (test-suite
      "Simple Satisfiability"
      (check-true (satisfiable-Leqs?
                   (list (-leq (-lexp (list 2 x))
                               x))))
      (check-false (satisfiable-Leqs?
                    (list (-leq (-lexp (list 2 x)) x)
                          (negate-prop
                           (-leq (-lexp (list 2 x)) x)))))
      (check-false (satisfiable-Leqs?
                    (list (-leq (-lexp 0) (-lexp (list 1 y)))
                          (negate-prop (-leq (-lexp 0) (-lexp (list 1 y)))))))

      )
     (test-suite
      "Leq binary relations"
      ;; contradictory-Leqs?
      (check-false (contradictory-Leqs?
                    (-leq (-lexp (list 2 x)) x)
                    (-leq (-lexp (list 2 x)) x)))
      (check-false (contradictory-Leqs?
                    (-leq (-lexp (list 2 x)) x)
                    (-leq (-lexp (list 2 y)) x)))
      (check-false (contradictory-Leqs?
                    (-leq (-lexp (list 2 x)) x)
                    (-leq x (-lexp (list 2 x)))))
      (check-true (contradictory-Leqs?
                   (-leq (-lexp (list 2 x)) x)
                   (negate-prop
                    (-leq (-lexp (list 2 x)) x))))
      (check-false (contradictory-Leqs?
                    (-leq (-lexp (list 2 x)) x)
                    (negate-prop
                     (-leq (-lexp (list 2 y)) y))))
      ;; complementary-Leqs?
      (check-false (complementary-Leqs?
                    (-leq (-lexp (list 2 x)) x)
                    (-leq (-lexp (list 2 x)) x)))
      (check-true (complementary-Leqs?
                   (-leq (-lexp (list 2 x)) x)
                   (-leq x (-lexp (list 2 x)))))
      (check-true (complementary-Leqs?
                   (-leq (-lexp (list 2 x)) x)
                   (negate-prop (-leq (-lexp (list 2 x)) x))))
      (check-false (complementary-Leqs?
                    (-leq (-lexp (list 2 x)) x)
                    (negate-prop (-leq (-lexp (list 2 y)) y))))

      )
     (test-suite
      "Leq implication"
      ;; x + y <= z; 0 <= y; 0 <= x --> x <= z /\ y <= z
      (check-true
       (Leqs-imply-Leqs?
        (list (-leq (-lexp (list 1 x) (list 1 y))
                    (-lexp (list 1 z)))
              (-leq (-lexp 0)
                    (-lexp (list 1 y)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x))))
        (list (-leq (-lexp (list 1 x))
                    (-lexp (list 1 z)))
              (-leq (-lexp (list 1 y))
                    (-lexp (list 1 z))))))

      ;; x + y <= z; 0 <= y; 0 <= x -/-> x <= z /\ y <= q
      (check-false
       (Leqs-imply-Leqs?
        (list (-leq (-lexp (list 1 x) (list 1 y))
                    (-lexp (list 1 z)))
              (-leq (-lexp 0)
                    (-lexp (list 1 y)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x))))
        (list (-leq (-lexp (list 1 x))
                    (-lexp (list 1 z)))
              (-leq (-lexp (list 1 y))
                    (-lexp (list 1 q))))))

      ;; 7x <= 29 --> x <= 4
      (check-true
       (Leqs-imply-Leqs?
        (list (-leq (-lexp (list 7 x))
                    (-lexp 29)))
        (list (-leq (-lexp (list 1 x))
                    (-lexp 4)))))
      ;; 7x <= 28 --> x <= 4
      (check-true
       (Leqs-imply-Leqs?
        (list (-leq (-lexp (list 7 x))
                    (-lexp 28)))
        (list (-leq (-lexp (list 1 x))
                    (-lexp 4)))))
      ;; 7x <= 28 does not --> x <= 3
      (check-false
       (Leqs-imply-Leqs?
        (list (-leq (-lexp (list 7 x))
                    (-lexp 28)))
        (list (-leq (-lexp (list 1 x))
                    (-lexp 3)))))


      ;; 7x <= 27 --> x <= 3
      (check-true
       (Leqs-imply-Leqs?
        (list (-leq (-lexp (list 7 x))
                    (-lexp 27)))
        (list (-leq (-lexp (list 1 x))
                    (-lexp 3)))))

      ;; 4x+3y+9z+20q-100r + 42 <= 4x+3y+9z+20q+100r;
      ;; x <= y + z;
      ;; 29r <= x + y + z + q;
      ;; 0 <= x;
      ;; 0 <= x + y + z;
      ;; 0 <= x + z;
      ;; x <= z
      ;; z + 1 <= t
      ;; 0 <= x + y;
      ;; 0 <= x + r;
      ;; 0 <= x + r + q;
      ;; -->
      ;; 0 <= t
      (check-true
       (Leqs-imply-Leqs?
        (list (-leq (-lexp (list 4 x) (list 3 y) (list 9 z) (list 20 q) (list -100 r) 42)
                    (-lexp (list 4 x) (list 3 y) (list 9 z) (list 20 q) (list 100 r)))
              (-leq (-lexp (list 1 x))
                    (-lexp (list 1 y) (list 1 z)))
              (-leq (-lexp (list 29 r))
                    (-lexp (list 1 x) (list 1 y) (list 1 z) (list 1 q)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x) (list 1 y) (list 1 z)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x) (list 1 z)))
              (-leq (-lexp (list 1 x))
                    (-lexp (list 1 z)))
              (-leq (-lexp (list 1 z) 1)
                    (-lexp (list 1 t)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x) (list 1 y)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x) (list 1 r)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x) (list 1 r) (list 1 q))))
        (list (-leq (-lexp 0)
                    (-lexp (list 1 t))))))

      ;; 4x+3y+9z+20q-100r + 42 <= 4x+3y+9z+20q+100r;
      ;; x <= y + z;
      ;; 29r <= x + y + z + q;
      ;; 0 <= x;
      ;; 0 <= x + y + z;
      ;; 0 <= x + z;
      ;; x <= z
      ;; z + 1 <= t
      ;; 0 <= x + y;
      ;; 0 <= x + r;
      ;; 0 <= x + r + q;
      ;; -/->
      ;; t <= 0
      (check-false
       (Leqs-imply-Leqs?
        (list (-leq (-lexp (list 4 x) (list 3 y) (list 9 z) (list 20 q) (list -100 r) 42)
                    (-lexp (list 4 x) (list 3 y) (list 9 z) (list 20 q) (list 100 r)))
              (-leq (-lexp (list 1 x))
                    (-lexp (list 1 y) (list 1 z)))
              (-leq (-lexp (list 29 r))
                    (-lexp (list 1 x) (list 1 y) (list 1 z) (list 1 q)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x) (list 1 y) (list 1 z)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x) (list 1 z)))
              (-leq (-lexp (list 1 x))
                    (-lexp (list 1 z)))
              (-leq (-lexp (list 1 z) 1)
                    (-lexp (list 1 t)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x) (list 1 y)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x) (list 1 r)))
              (-leq (-lexp 0)
                    (-lexp (list 1 x) (list 1 r) (list 1 q))))
        (list (-leq (-lexp (list 1 t))
                    (-lexp 0))))))
     ))
  )
