#lang racket/base
(require "utils.rkt"
         (contract-req)
         racket/match
         racket/list
         (for-syntax racket/base racket/match))

;; Lightweight variant of free-id-sets

(provide fid-hset
         make-fid-hset
         fid-hset-count
         fid-hset-empty?
         fid-hset-add
         fid-hset-add!
         fid-hset-member?
         fid-hset-remove
         fid-hset-union
         fid-hset-map
         fid-hset-copy
         fid-hset-remove!
         in-fid-hset-buckets
         in-fid-hset-bucket
         for/fid-hset
         for*/fid-hset
         ;for/mutable-fid-hset
         ;for*/mutable-fid-hset
         )

(provide-for-cond-contract fid-hset?)

(define-for-cond-contract fid-hset? (hash/c symbol? (listof identifier?) #:immutable #t #:flat? #t))

(define fid-hset
  (case-lambda
    [() #hash()]
    [l (let loop ([h #hash()]
                  [args l])
         (match args
           [(list) h]
           [(cons id rst)
            (loop (fid-hset-add h id) rst)]))]))

(define make-fid-hset
  (case-lambda
    [() (make-hash)]
    [args (let ([h (make-hash)])
            (for ([id (in-list args)])
              (fid-hset-add! h id))
            h)]))

(define-syntax fid-hset-empty? (make-rename-transformer #'hash-empty?))

(define (fid-hset-count h)
  (for/sum ([l (in-fid-hset-buckets h)])
    (length l)))


;; fid-hset-add
(define (fid-hset-add h x)
  (define x-sym (syntax-e x))
  (cond
    [(hash-ref h x-sym #f)
     => (λ (l) (if (member x l free-identifier=?)
                   h
                   (hash-set h x-sym (cons x l))))]
    [else (hash-set h x-sym (list x))]))


(define (fid-hset-add! h x)
  (define x-sym (syntax-e x))
  (cond
    [(hash-ref h x-sym #f)
     => (λ (l) (unless (member x l free-identifier=?)
                 (hash-set! h x-sym (cons x l))))]
    [else (hash-set! h x-sym (list x))]))

;; fid-hset-member?
(define (fid-hset-member? h x)
  (cond
    [(hash-ref h (syntax-e x) #f)
     => (λ (l) (and (member x l free-identifier=?) #t))]
    [else #f]))


;; fid-hset-remove
(define (fid-hset-remove h x)
  (define x-sym (syntax-e x))
  (cond
    [(hash-ref h x-sym #f)
     => (λ (l)
          (let ([l (remf (λ (y) (free-identifier=? x y)) l)])
            (if (null? l)
                (hash-remove h x-sym)
                (hash-set h x-sym l))))]
    [else h]))

(define (fid-hset-remove! h x)
  (define x-sym (syntax-e x))
  (cond
    [(hash-ref h x-sym #f)
     => (λ (l)
          (let ([l (remf (λ (y) (free-identifier=? x y)) l)])
            (if (null? l)
                (hash-remove! h x-sym)
                (hash-set! h x-sym l))))]
    [else (void)]))


(define (fid-hset-union s1 s2)
  (if (< (hash-count s1) (hash-count s2))
      (fid-hset-union s2 s1)
      (for*/fold ([s1 s1])
                 ([b (in-fid-hset-buckets s2)]
                  [id (in-fid-hset-bucket b)])
        (fid-hset-add s1 id))))

(define (fid-hset-map s f)
  (for*/fold ([l '()])
             ([b (in-fid-hset-buckets s)]
              [id (in-fid-hset-bucket b)])
    (cons (f id) l)))

;; fid-hash iterating forms
(define-syntax in-fid-hset-buckets (make-rename-transformer #'in-hash-values))
(define-syntax in-fid-hset-bucket (make-rename-transformer #'in-list))


;; for/fid-hash
(define-syntax-rule (for/fid-hset bindings body ...)
  (for/fold ([h (fid-hset)])
            bindings
    (fid-hset-add h (let () body ...))))


;; for*/fid-hash
(define-syntax-rule (for*/fid-hset bindings body ...)
  (for*/fold ([h (fid-hset)])
             bindings
    (fid-hset-add h (let () body ...))))

;; fid-hset-copy
(define-syntax fid-hset-copy (make-rename-transformer #'hash-copy))


(module+ test
  (module sub1 racket/base
    (provide x1)
    (define x 42)
    (define x1 #'x))
  (require 'sub1)

  (module sub2 racket/base
    (provide x2)
    (define x 42)
    (define x2 #'x))
  (require 'sub2)
  (define x #'x)
  (define y #'y)

  (define-syntax (assert stx)
    (syntax-case stx ()
      [(_ expr)
       #`(unless expr #,(quasisyntax/loc stx (error 'assert "failed!")))]))

  ;; tests for immutable fid-hset features
  (let ()
    (define h (fid-hset))
    (assert (fid-hset-empty? h))
    (assert (= 0 (fid-hset-count h)))

    (set! h (fid-hset-add h x))
    (assert (not (fid-hset-empty? h)))
    (assert (= 1 (fid-hset-count h)))
    (assert (fid-hset-member? h x))
    (assert (not (fid-hset-member? h x1)))

    (set! h (fid-hset-add h x1))
    (set! h (fid-hset-add h x2))
    (set! h (fid-hset-add h y))
    (assert (not (fid-hset-empty? h)))
    
    (assert (= 4 (fid-hset-count h)))
    (assert (fid-hset-member? h x))
    (assert (fid-hset-member? h x1))
    (assert (fid-hset-member? h x2))
    (assert (fid-hset-member? h y))

    (set! h (fid-hset-remove h x2))
    (assert (not (fid-hset-empty? h)))
    (assert (= 3 (fid-hset-count h)))
    (assert (fid-hset-member? h x))
    (assert (fid-hset-member? h x1))
    (assert (fid-hset-member? h y))
  
    (set! h (for*/fid-hset ([b (in-fid-hset-buckets h)]
                            [id (in-fid-hset-bucket b)]
                            #:when (eq? 'x (syntax-e id)))
              id))
  
    (assert (not (fid-hset-empty? h)))
    (assert (= 2 (fid-hset-count h)))
    (assert (fid-hset-member? h x))
    (assert (fid-hset-member? h x1))
    (assert (not (fid-hset-member? h y))))

  ;; tests for mutable fid-hset
  (let ()
    (define h (make-fid-hset))
    (assert (fid-hset-empty? h))
    (assert (= 0 (fid-hset-count h)))

    (fid-hset-add! h x)
    (assert (not (fid-hset-empty? h)))
    (assert (= 1 (fid-hset-count h)))
    (assert (fid-hset-member? h x))
    (assert (not (fid-hset-member? h x1)))

    (fid-hset-add! h x1)
    (fid-hset-add! h x2)
    (fid-hset-add! h y)
    
    (assert (not (fid-hset-empty? h)))
    (assert (= 4 (fid-hset-count h)))
    (assert (fid-hset-member? h x))
    (assert (fid-hset-member? h x1))
    (assert (fid-hset-member? h x2))
    (assert (fid-hset-member? h y))

    (fid-hset-remove! h x2)
    (assert (not (fid-hset-empty? h)))
    (assert (= 3 (fid-hset-count h)))
    (assert (fid-hset-member? h x))
    (assert (fid-hset-member? h x1))
    (assert (fid-hset-member? h y))
    (assert (not (fid-hset-member? h x2)))
  
    (fid-hset-remove! h y)
    (assert (not (fid-hset-empty? h)))
    (assert (= 2 (fid-hset-count h)))
    (assert (fid-hset-member? h x))
    (assert (fid-hset-member? h x1))
    (assert (not (fid-hset-member? h y)))))


