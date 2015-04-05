#lang typed/racket

(require dependentfuns)

(define v : (Vectorof Integer) (vector 42 42 42 42))


(define (test-1a [i : (Refine [n : Integer]
                             (and (<= 0 i)
                                  (< i (len v))))])
  (safe-vector-ref v i))

(define (test-1b [i : (Refine [n : Integer]
                              (and (>= i 0)
                                   (> (len v) i)))])
  (safe-vector-ref v i))

(define (test0 [i : (Refine [n : Integer]
                            (< i (len v)))])
  (if (int<= 0 i)
      (safe-vector-ref v i)
      "Hi"))

(define (test1 [i : Integer])
  (if (and (int< i (exact-vector-length v))
           (int<= 0 i))
      (safe-vector-ref v i)
      "Hi"))


(define (test2 [n : Integer]
             [m : Integer])
  (if (and (int<= 0 m)
           (int<= 0 n)
           (int< (int+ n m) (exact-vector-length v)))
      (values (safe-vector-ref v n)
              (safe-vector-ref v m))
      (values "Hi" "there")))


(define (test3 [n : Integer]
             [m : Integer])
  (if (and (int<= 0 n)
           (int<= n (int*2 m))
           (int< (int*3 m) (exact-vector-length v)))
      (values (safe-vector-ref v n)
              (safe-vector-ref v (int*2 m)))
      (values "Hi" "there")))


(define (test4 [i : Integer])
  (cond [(or (int<= (exact-vector-length v) i)
             (int< i 0))
         "Hi"]
        [else (safe-vector-ref v i)]))


(define (test5 [n : Integer]
               [m : Integer])
  (cond
    [(int< n 0)
     (values "hello" "world")]
    [(int< (int*2 m) n)
     (values "hello" "there")]
    [(int<= (exact-vector-length v) (int*3 m))
     (values "!" "!")]
    [else
     (values (safe-vector-ref v n)
             (safe-vector-ref v (int*2 m)))]))

;; produces:
;; 42
;; 42
;; 42
;; 42
;; 42