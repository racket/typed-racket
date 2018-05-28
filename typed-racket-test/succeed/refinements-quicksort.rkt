#lang typed/racket/base #:with-refinements

(: safe-vector-ref
   (All (A) (-> ([v : (Vectorof A)]
                 [n : Integer])
                #:pre (v n) (<= 0 n (- (vector-length v) 1))
                A)))
(define safe-vector-ref vector-ref)

(: safe-vector-set!
   (All (A) (-> ([v : (Vectorof A)]
                 [n : Natural]
                 [a : A])
                #:pre (v n) (< n (vector-length v))
                Void)))
(define safe-vector-set! vector-set!)


(: quicksort! (-> (Vectorof Real) Void))
(define (quicksort! A)
  (quicksort-helper! A 0 (- (vector-length A) 1)))

(: quicksort-helper! (-> ([A : (Vectorof Real)]
                   [lo : Natural]
                   [hi : Integer])
                  #:pre (A lo hi)
                  (and (<= lo (vector-length A))
                       (< hi (vector-length A)))
                  Void))
(define (quicksort-helper! A lo hi)
  (when (< lo hi)
    (define pivot (partition! A lo hi))
    (quicksort-helper! A lo (- pivot 1))
    (quicksort-helper! A (+ pivot 1) hi)))


(: swap! (-> ([A : (Vectorof Real)]
              [i : Natural]
              [j : Natural])
             #:pre (A i j)
             (and (< i (vector-length A))
                  (< j (vector-length A)))
             Void))
(define (swap! A i j)
  (define tmp (safe-vector-ref A i))
  (safe-vector-set! A i (safe-vector-ref A j))
  (safe-vector-set! A j tmp)) 



(: partition! (-> ([A : (Vectorof Real)]
                   [lo : Natural]
                   [hi : Natural])
                  #:pre (A lo hi)
                  (< lo hi (vector-length A))
                  (Refine [pivot : Natural]
                          (<= lo pivot hi))))
(define (partition! A lo hi)
  (define pivot (safe-vector-ref A lo))
  (let outer-loop!
    ([i : (Refine [n : Natural] (< lo n))
        (+ 1 lo)]
     [j : (Refine [n : Natural] (and (<= n hi)
                                     (<= lo n))) hi])
    (let i-loop!
      ([i : (Refine [n : Natural] (< lo n))
          i])
      (cond
        [(and (<= i j)
              (< (safe-vector-ref A i) pivot))
         (i-loop! (+ i 1))]
        [else
         (let j-loop!
           ([j : (Refine [n : Natural] (and (<= n hi)
                                            (<= lo n))) j])
           (cond
             [(and (>= (safe-vector-ref A j) pivot)
                   (>= j i))
              (j-loop! (- j 1))]
             [(> i j) (swap! A lo j)
                      j]
             [else
              (swap! A i j)
              (outer-loop! i j)]))]))))


(: random-reals (-> Integer (Listof Real)))
(define (random-reals n)
  (build-list n (λ _ (ann (random 10000) Real))))

(for ([_ (in-range 10000)])
  (for ([size (in-range 23)])
    (define l (random-reals size))
    (define v (list->vector l))
    (quicksort! v)
    (define l* (vector->list v))
    (unless (equal? (sort l <) l*)
      (error "bad sort!: \n  expected: ~a\n  got: ~a\n\n"
             l l*))))

(define v : (Vectorof Real) (vector 0 5 9 12 3 0 4 6 3))
(quicksort! v)
v



(: misc-inf-loop (-> (Refine [n : Natural] (<= n 11))
                     (Listof (Refine [n : Natural] (<= n 11)))))
(define (misc-inf-loop x)
  (cond
    [(or (= x 1) (= x 6)) (list x)]
    [else (list)]))


(ann (add1 1) (Refine [n : Integer] (= n 2)))
(ann (sub1 5) (Refine [n : Integer] (= n 4)))
(ann (random 5) (Refine [n : Integer] (<= 0 n 4)))
(ann (random 1 5) (Refine [n : Integer] (<= 1 n 4)))
(ann (add1 (random 5)) (Refine [n : Integer] (<= 1 n 6)))
(ann (modulo 1 5) (Refine [n : Integer] (<= 0 n 4)))

(: foo (-> Integer (Refine [n : Integer] (= n 42))))
(define (foo x)
  (if (equal? x 42)
      x
      42))

(: bar (-> Integer (Refine [n : Integer] (= n 42))))
(define (bar x)
  (if (eqv? x 42)
      x
      42))

(: baz (-> Integer (Refine [n : Integer] (= n 42))))
(define (baz x)
  (if (eq? x 42)
      x
      42))
