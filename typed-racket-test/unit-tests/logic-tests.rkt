#lang racket/base

(require "test-utils.rkt"
         rackunit racket/format racket/match racket/list
         (typecheck tc-metafunctions tc-subst)
         (rep filter-rep type-rep object-rep)
         (types abbrev union filter-ops tc-result numeric-tower)
         (logic proves)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)

(define-syntax-rule (SLI-add-test new-sli slis expected-slis)
  (let* ([slis* (SLI-add new-sli slis)]
         [slis*-perms (permutations slis*)]
         [expected-perms (permutations expected-slis)])
    (check-true (format "~a" '(new-sli slis))
                (or (for*/or ([slis* (in-list slis*-perms)]
                              [exp (in-list expected-perms)])
                      (filter-equal? slis* exp))
                    (list 'expected expected-slis 'actual slis*)))))

(define-syntax-rule (SLI-imp assumptions goals)
  (for/and ([goal (in-list goals)])
      (simple-proves assumptions goal)))

(define-syntax-rule (SLI-imp/and assumptions goals)
  (simple-proves assumptions (apply -and goals)))

(define (id sym)
  (-id-path (datum->syntax #f sym)))

;; some propositions w/ no logical overlap
;; and no interesting types
(define ¬ invert-filter)
(define -><- -bot)
(define P (-filter -Symbol (id (gensym))))
(define ¬P (¬ P))
(define Q (-filter -Symbol (id (gensym))))
(define ¬Q (¬ Q))
(define R (-filter -Symbol (id (gensym))))
(define ¬R (¬ R))
(define S (-filter -Symbol (id (gensym))))
(define ¬S (¬ S))
(define (gen-prop)
  (-filter -Symbol (id (gensym))))

(define 
  tests
  (test-suite 
   "Logic"
   
   (test-suite
    "Basic Rules of Inference"
    (check-not-false
     (simple-proves (list P)
                    P)
     "Axiom")
    (check-false
     (simple-proves null
                    -><-)
     "Consistency")
    (check-not-false
     (simple-proves (list -><-)
                    (-and P Q R S))
     "Ex Falso Quodlibet")
    (check-not-false
     (simple-proves (list P (-imp P Q))
                    Q)
     "Modus Ponens")
    (check-not-false
     (simple-proves (list (-imp P Q) ¬Q)
                    ¬P)
     "Modus tollens")
    (check-not-false
     (simple-proves (list P Q)
                    (-and P Q))
     "Conjunction")
    (check-not-false
     (simple-proves (list (-and P Q R S)) 
                    R)
     "Simplification")
    (check-not-false
     (simple-proves (list R) 
                    (-or P Q R S))
     "Addition")
    (check-not-false
     (simple-proves (list (-imp P Q) (-imp R Q) (-or P R)) 
                    Q)
     "Disjunctive Elimination")
    (check-not-false
     (simple-proves (list (-or P Q) ¬P) 
                    Q)
     "Disjunctive Syllogism")
    (check-not-false
     (simple-proves (list (-imp P Q) (-imp Q R)) 
                    (-imp P R))
     "Hypothetical Syllogism")
    (check-not-false
     (simple-proves (list (-imp P Q) (-imp R S) (-or P R)) 
                    (-or Q S))
     "Constructive Dilemma")
    (check-not-false
     (simple-proves (list (-imp P Q) (-imp R S) (-or ¬Q ¬S)) 
                    (-or ¬P ¬R))
     "Destructive Dilemma")
    (check-not-false
     (simple-proves (list (-imp P Q)) 
                    (-imp P (-and P Q)))
     "Absorption")
    (check-not-false
     (simple-proves (list (-or P P)) 
                    P)
     "Tautology")
    (check-not-false
     (simple-proves (list (-imp P Q)) 
                    (-imp ¬Q ¬P))
     "Transposition1")
    (check-not-false
     (simple-proves (list (-imp ¬Q ¬P)) 
                    (-imp P Q))
     "Transposition2")
    (check-not-false
     (simple-proves (list (-imp P (-imp Q R))) 
                    (-imp (-and P Q) R))
     "Exportation1")
    (check-not-false
     (simple-proves (list (-imp (-and P Q) R)) 
                    (-imp P (-imp Q R)))
     "Exportation2")
    (check-not-false
     (simple-proves (list (¬ (-imp P Q))) 
                    (-and P ¬Q))
     "Conditional Negation1")
    (check-not-false
     (simple-proves (list (-and P ¬Q)) 
                    (¬ (-imp P Q)))
     "Conditional Negation2"))
   
   (test-suite
    "Simple Invalid Implications"
    (check-false
     (simple-proves (list (-or P Q R) ¬Q)
                    R))
    (check-false
     (simple-proves (list P Q R)
                    (-and P ¬P)))
    (check-false
     (simple-proves (list (-filter -Nat (id 'x))
                          (-not-filter -NegInt (id 'x))
                          (-filter -Number (id 'x)))
                    (-filter -NegInt (id 'x)))))
   
   (test-suite 
    "Misc Proofs"
    (check-not-false
     (simple-proves (list (-filter -Nat #'x)) (-filter -Nat #'x)))
    (check-false
     (simple-proves (list (-filter -Nat #'x)) (-filter -String #'x)))
    (check-not-false
     (simple-proves (list (-filter -Nat #'x)) (-filter -Number #'x)))
    (check-false
     (simple-proves (list (-filter -Number #'x)) (-filter -Nat #'x)))
    (check-not-false
     (simple-proves (list (-or (-and (-filter -Number #'x)
                                     (-filter -Number #'y))
                               (-and (-filter -String #'x)
                                     (-filter -String #'y)))
                          (-not-filter -Number #'y)) 
                    (-and (-filter -String #'x)
                          (-not-filter -Number #'x))))
    (check-not-false
     (simple-proves (list (-or (-and (-filter -Number #'x)
                                     (-filter -Number #'y))
                               (-and (-filter -String #'x)
                                     (-filter -String #'y)
                                     (-filter (Un -String -Nat) #'z)))
                          (-not-filter -Number #'y)
                          (-not-filter -Number #'z)
                          (-imp (-filter -String #'z)
                                (-filter -String #'a)))
                    (-filter -String #'a)))
    )
   
   (test-suite
    "SLI generation"
    ;; 4 ≤ 3 is contradictory
    (check-match (-sli (-leq (-id-lexp 4)
                             (-id-lexp 3)))
                 (list (? Bot?)))
    ;; 3 ≤ 4 is trivially valid
    (check-match (-sli (-leq (-id-lexp 3)
                             (-id-lexp 4)))
                 (list (? Top?)))
    ;; 1 ≤ 1 ∧ 10 ≤ 20 is trivially valid
    (check-match (-sli (-leq (-id-lexp 1)
                             (-id-lexp 1))
                       (-leq (-id-lexp 10)
                             (-id-lexp 20)))
                 (list (? Top?) (? Top?)))
    ;; 0 ≤ y ∧ y ≤ -1 is contradictory
    (check-match (-sli (-leq (-id-lexp 0)
                             (-id-lexp (1 y)))
                       (-leq (-id-lexp (1 y))
                             (-id-lexp -1)))
                 (list (? Bot?)))
    ;; x ≤ z should produce 1 SLI
    (check-match (-sli (-leq (-id-lexp (1 x))
                             (-id-lexp (1 z))))
                 (list (? SLI?)))
    ;; x ≤ z ∧ 2y ≤ 3q should produce 2 SLIs
    (check-match (-sli (-leq (-id-lexp (1 x))
                             (-id-lexp (1 z)))
                       (-leq (-id-lexp (2 y))
                             (-id-lexp (3 q))))
                 (list (? SLI?) (? SLI?)))
    ;; x ≤ z ∧ y ≤ q ∧ y ≤ r should produce 2 SLIs
    (check-match (-sli (-leq (-id-lexp (1 x))
                             (-id-lexp (1 z)))
                       (-leq (-id-lexp (1 y))
                             (-id-lexp (1 q)))
                       (-leq (-id-lexp (1 y))
                             (-id-lexp (1 r))))
                 (list (? SLI?) (? SLI?)))
    ;; x ≤ z ∧ y ≤ q ∧ y ≤ r ∧ x ≤ r should produce 1 SLI
    (check-match (-sli (-leq (-id-lexp (1 x))
                             (-id-lexp (1 z)))
                       (-leq (-id-lexp (1 y))
                             (-id-lexp (1 q)))
                       (-leq (-id-lexp (1 y))
                             (-id-lexp (1 r)))
                       (-leq (-id-lexp (1 x))
                             (-id-lexp (1 r))))
                 (list (? SLI?))))
   
   (test-suite
    "SLI add")
   
   (test-suite
    "SLI proofs"
    
    ;; 4 <= 3 is false
    (check-false (SLI-imp (-sli)
                          (-sli (-leq (-id-lexp 4)
                                      (-id-lexp 3)))))
    ;; P and ~P --> false
    (check-not-false (SLI-imp (-sli (-leq (-id-lexp) (-id-lexp (1 a)))
                                    (leq-negate (-leq (-id-lexp) (-id-lexp (1 a)))))
                              (-sli (-leq (-id-lexp 4)
                                          (-id-lexp 3)))))
    
    
    ;; x + y <= z; 0 <= y; 0 <= x --> x <= z /\ y <= z
    (check-not-false (SLI-imp (-sli (-leq (-id-lexp (1 x) (1 y))
                                          (-id-lexp (1 z)))
                                    (-leq (-id-lexp)
                                          (-id-lexp (1 y)))
                                    (-leq (-id-lexp)
                                          (-id-lexp (1 x))))
                              (-sli (-leq (-id-lexp (1 x))
                                          (-id-lexp (1 z)))
                                    (-leq (-id-lexp (1 y))
                                          (-id-lexp (1 z))))))
    
    ;; x + y <= z; 0 <= y; 0 <= x -/-> x <= z /\ y <= q
    (check-false (SLI-imp (-sli (-leq (-id-lexp (1 x) (1 y))
                                      (-id-lexp (1 z)))
                                (-leq (-id-lexp)
                                      (-id-lexp (1 y)))
                                (-leq (-id-lexp)
                                      (-id-lexp (1 x))))
                          (-sli (-leq (-id-lexp (1 x))
                                      (-id-lexp (1 z)))
                                (-leq (-id-lexp (1 y))
                                      (-id-lexp (1 q))))))
    
    ;; 7x <= 29 --> x <= 4
    (check-not-false (SLI-imp (-sli (-leq (-id-lexp (7 x))
                                          (-id-lexp 29)))
                              (-sli (-leq (-id-lexp (1 x))
                                          (-id-lexp 4)))))
    ;; 7x <= 28 --> x <= 4
    (check-not-false (SLI-imp (-sli (-leq (-id-lexp (7 x))
                                          (-id-lexp 28)))
                              (-sli (-leq (-id-lexp (1 x))
                                          (-id-lexp 4)))))
    ;; 7x <= 28 does not --> x <= 3
    (check-false (SLI-imp (-sli (-leq (-id-lexp (7 x))
                                      (-id-lexp 28)))
                          (-sli (-leq (-id-lexp (1 x))
                                      (-id-lexp 3)))))
    
    
    ;; 7x <= 27 --> x <= 3
    (check-not-false (SLI-imp (-sli (-leq (-id-lexp (7 x))
                                          (-id-lexp 27)))
                              (-sli (-leq (-id-lexp (1 x))
                                          (-id-lexp 3)))))
    
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
    (check-not-false (SLI-imp (-sli (-leq (-id-lexp (4 x) (3 y) (9 z) (20 q) (-100 r) 42)
                                          (-id-lexp (4 x) (3 y) (9 z) (20 q) (100 r)))
                                    (-leq (-id-lexp (1 x))
                                          (-id-lexp (1 y) (1 z)))
                                    (-leq (-id-lexp (29 r))
                                          (-id-lexp (1 x) (1 y) (1 z) (1 q)))
                                    (-leq (-id-lexp)
                                          (-id-lexp (1 x)))
                                    (-leq (-id-lexp)
                                          (-id-lexp (1 x) (1 y) (1 z)))
                                    (-leq (-id-lexp)
                                          (-id-lexp (1 x) (1 z)))
                                    (-leq (-id-lexp (1 x))
                                          (-id-lexp (1 z)))
                                    (-leq (-id-lexp (1 z) 1)
                                          (-id-lexp (1 t)))
                                    (-leq (-id-lexp)
                                          (-id-lexp (1 x) (1 y)))
                                    (-leq (-id-lexp)
                                          (-id-lexp (1 x) (1 r)))
                                    (-leq (-id-lexp)
                                          (-id-lexp (1 x) (1 r) (1 q))))
                              (-sli (-leq (-id-lexp)
                                          (-id-lexp (1 t))))))
    
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
    (check-false (SLI-imp (-sli (-leq (-id-lexp (4 x) (3 y) (9 z) (20 q) (-100 r) 42)
                                      (-id-lexp (4 x) (3 y) (9 z) (20 q) (100 r)))
                                (-leq (-id-lexp (1 x))
                                      (-id-lexp (1 y) (1 z)))
                                (-leq (-id-lexp (29 r))
                                      (-id-lexp (1 x) (1 y) (1 z) (1 q)))
                                (-leq (-id-lexp)
                                      (-id-lexp (1 x)))
                                (-leq (-id-lexp)
                                      (-id-lexp (1 x) (1 y) (1 z)))
                                (-leq (-id-lexp)
                                      (-id-lexp (1 x) (1 z)))
                                (-leq (-id-lexp (1 x))
                                      (-id-lexp (1 z)))
                                (-leq (-id-lexp (1 z) 1)
                                      (-id-lexp (1 t)))
                                (-leq (-id-lexp)
                                      (-id-lexp (1 x) (1 y)))
                                (-leq (-id-lexp)
                                      (-id-lexp (1 x) (1 r)))
                                (-leq (-id-lexp)
                                      (-id-lexp (1 x) (1 r) (1 q))))
                          (-sli (-leq (-id-lexp (1 t))
                                      (-id-lexp))))))
     
   ))
