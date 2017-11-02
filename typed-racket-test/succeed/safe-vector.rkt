#lang typed/racket/base #:with-refinements

(require typed/racket/unsafe
         racket/unsafe/ops)

(provide (all-defined-out))

(: safe-vector-ref (All (A) (-> ([v : (Vectorof A)]
                                 [n : Natural])
                                #:pre (v n) (< n (vector-length v))
                                A)))
(define safe-vector-ref unsafe-vector-ref)

(: safe-vector-set! (All (A) (-> ([v : (Vectorof A)]
                                  [n : Natural]
                                  [a : A])
                                 #:pre (v n) (< n (vector-length v))
                                 Void)))
(define safe-vector-set! unsafe-vector-set!)

(: build-vector
   (All (A) (-> ([n : Natural]
                 [proc : (n) (-> (Refine [i : Natural] (< i n)) A)])
                (Refine [v : (Vectorof A)]
                        (= n (vector-length v))))))
(define (build-vector n proc)
  (cond
    [(> n 0)
     (define init-val (proc 0))
     (define vec (make-vector n init-val))
     (let loop! ([i : Natural 1])
       (when (< i n)
         (safe-vector-set! vec i (proc i))
         (loop! (add1 i))))
     vec]
    [else (vector)]))





(: vector-map
   (All (A B) (-> ([proc : (-> A B)]
                   [vec : (Vectorof A)])
                  (Vectorof B))))
(define (vector-map proc vec)
  (build-vector (vector-length vec)
                (λ ([i : (Refine [i : Natural]
                                 (< i (vector-length vec)))])
                  (proc (safe-vector-ref vec i)))))

(: vector-map!
   (All (A) (-> ([proc : (-> A A)]
                 [vec : (Vectorof A)])
                Void)))
(define (vector-map! proc vec)
  (define len (vector-length vec))
  (let loop! ([i : Natural 0])
    (when (< i len)
      (define a (safe-vector-ref vec i))
      (safe-vector-set! vec i (proc a))
      (loop! (+ 1 i)))))



(: vector-copy
   (All (A) (-> ([vec : (Vectorof A)]
                 [start : Natural]
                 [end : Natural])
                #:pre (vec start end)
                (<= start end (vector-length vec))
                (Refine [res : (Vectorof A)]
                        (= (- end start)
                           (vector-length res))))))
(define (vector-copy vec start end)
  (define len (- end start))
  (cond
    [(= 0 len) (vector)]
    [else
     (define res (make-vector len (safe-vector-ref vec start)))
     (let loop! ([i : Natural 0])
       (when (< i len)
         (define a (safe-vector-ref vec (+ start i)))
         (safe-vector-set! res i a)
         (loop! (+ 1 i))))
     res]))


(: vector-copy!
   (All (A) (-> ([dest : (Vectorof A)]
                 [dest-start : Natural]
                 [src : (Vectorof A)]
                 [src-start : Natural]
                 [src-end : Natural])
                #:pre (dest dest-start src src-start src-end)
                (and (<= dest-start (vector-length dest))
                     (<= src-start src-end (vector-length src))
                     (<= (- src-end src-start)
                         (- (vector-length dest) dest-start)))
                Void)))
(define (vector-copy! dest dest-start src src-start src-end)
  (define count (- src-end src-start))
  (let loop! ([i : Natural 0])
    (when (< i count)
      (define a (safe-vector-ref src (+ src-start i)))
      (safe-vector-set! dest (+ dest-start i) a)
      (loop! (+ 1 i)))))




(: vector-append
   (All (A) (-> ([vec1 : (Vectorof A)]
                 [vec2 : (Vectorof A)])
                (Refine [res : (Vectorof A)]
                        (= (vector-length res)
                           (+ (vector-length vec1)
                              (vector-length vec2)))))))
(define (vector-append v1 v2)
  (define len1 (vector-length v1))
  (cond
    [(= 0 len1) (vector-copy v2 0 (vector-length v2))]
    [else
     (define len2 (vector-length v2))
     (define res (make-vector (+ len1 len2)
                              (safe-vector-ref v1 0)))

     (let loop! ([i : Natural 1])
       (when (< i len1)
         (safe-vector-set! res
                           i
                           (safe-vector-ref v1 i))
         (loop! (add1 i))))
     (let loop! ([i : Natural len1])
       (when (< i len2)
         (safe-vector-set! res
                           (+ len1 i)
                           (safe-vector-ref v2 i))
         (loop! (add1 i))))
     res]))



(: vector-take
   (All (A) (-> ([vec : (Vectorof A)]
                 [pos : Natural])
                #:pre (vec pos) (<= pos (vector-length vec))
                (Refine [ret : (Vectorof A)]
                        (= (vector-length ret) pos)))))
(define (vector-take vec pos)
  (vector-copy vec 0 pos))


(: vector-drop
   (All (A) (-> ([vec : (Vectorof A)]
                 [pos : Natural])
                #:pre (vec pos) (<= pos (vector-length vec))
                (Refine [ret : (Vectorof A)]
                        (= (vector-length ret)
                           (- (vector-length vec) pos))))))
(define (vector-drop vec pos)
  (vector-copy vec pos (vector-length vec)))



(: vector-split-at
   (All (A) (-> ([vec : (Vectorof A)]
                 [pos : Natural])
                #:pre (vec pos) (<= pos (vector-length vec))
                (values (Refine [res1 : (Vectorof A)]
                                (= (vector-length res1) pos))
                        (Refine [res2 : (Vectorof A)]
                                (= (vector-length res2)
                                   (- (vector-length vec) pos)))))))
(define (vector-split-at vec pos)
  (values (vector-copy vec 0 pos)
          (vector-copy vec pos (vector-length vec))))



(: vector-take-right
   (All (A) (-> ([vec : (Vectorof A)]
                 [pos : Natural])
                #:pre (vec pos) (<= pos (vector-length vec))
                (Refine [res : (Vectorof A)]
                        (= (vector-length res) pos)))))
(define (vector-take-right vec pos)
  (define len (vector-length vec))
  (vector-copy vec (- len pos) len))


(: vector-drop-right
   (All (A) (-> ([vec : (Vectorof A)]
                 [pos : Natural])
                #:pre (vec pos) (<= pos (vector-length vec))
                (Refine [res : (Vectorof A)]
                        (= (vector-length res)
                           (- (vector-length vec) pos))))))
(define (vector-drop-right vec pos)
  (define len (vector-length vec))
  (vector-copy vec 0 (- len pos)))


(: vector-split-at-right
   (All (A) (-> ([vec : (Vectorof A)]
                 [pos : Natural])
                #:pre (vec pos) (<= pos (vector-length vec))
                (values (Refine [res1 : (Vectorof A)]
                                (= (vector-length res1)
                                   (- (vector-length vec) pos)))
                        (Refine [res2 : (Vectorof A)]
                                (= (vector-length res2) pos))))))
(define (vector-split-at-right vec pos)
  (define len (vector-length vec))
  (values (vector-copy vec 0 (- len pos))
          (vector-copy vec (- len pos) len)))
