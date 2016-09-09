#lang racket/base

(require racket/unsafe/ops
         racket/format)

(provide primitive<=?)

;; vector of predicates for primitives paired with
;; functions which compair primitives of the same type
(define prims
  (vector  (cons (λ (v) (eq? v #t)) (λ _ #t))
           (cons (λ (v) (eq? v #f)) (λ _ #t))
           (cons (λ (v) (eq? v '())) (λ _ #t))
           (cons real? <=)
           (cons complex? (λ (x y) (and (<= (real-part x)
                                            (real-part y))
                                        (<= (imag-part x)
                                            (imag-part y)))))
           (cons char? char<=?)
           (cons string? string<=?)
           (cons bytes? (λ (b1 b2) (or (bytes<? b1 b2)
                                       (bytes=? b1 b2))))
           (cons symbol? (λ (s1 s2) (or (symbol<? s1 s2)
                                        (eq? s1 s2))))
           (cons keyword? (λ (k1 k2) (or (keyword<? k1 k2)
                                         (eq? k1 k2))))
           (cons pair? (λ (p1 p2) (and (primitive<=? (car p1) (car p2))
                                       (primitive<=? (cdr p1) (cdr p2)))))
           (cons vector? (λ (v1 v2) (and (< (vector-length v1) (vector-length v2))
                                         (and (= (vector-length v1) (vector-length v2))
                                              (for/and ([val1 (in-vector v1)]
                                                        [val2 (in-vector v2)])
                                                (primitive<=? val1 val2))))))
           (cons box? (λ (b1 b2) (primitive<=? (unbox b1) (unbox b2))))))

;; finds which index into prims 's' belongs
(define (index-of s)
  (let loop ([i 0])
    (cond
      [(unsafe-fx<= i (vector-length prims))
       (if ((unsafe-car (unsafe-vector-ref prims i)) s)
           i
           (loop (unsafe-fx+ i 1)))]
      [else (error 'index-of "impossible!")])))

;; compares two primitives (i.e. sexps) based on the ordering
;; implied by the 'prims' predicates and, if they are equal types of
;; primitives, on the functions contained in prims
(define (primitive<=? s1 s2)
  (define idx1 (index-of s1))
  (define idx2 (index-of s2))
  (cond
    [(unsafe-fx< idx1 idx2) #t]
    [(unsafe-fx> idx1 idx2) #f]
    [else ((unsafe-cdr (vector-ref prims idx1)) s1 s2)]))
