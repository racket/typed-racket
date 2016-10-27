#lang racket/base
(require "utils.rkt"
         (contract-req)
         racket/match
         racket/list
         (for-syntax racket/base racket/match))

;; Lightweight variant of free-id-tables

(provide fid-hash
         make-fid-hash
         fid-hash-count
         fid-hash-empty?
         fid-hash-set
         fid-hash-set!
         fid-hash-ref
         fid-hash-remove
         fid-hash-remove!
         fid-hash-copy
         in-fid-hash-buckets
         in-fid-hash-bucket
         for/fid-hash
         for*/fid-hash
         for/mutable-fid-hash
         for*/mutable-fid-hash
         fid-hash-keys
         fid-hash-values)

(provide-for-cond-contract fid-hashof)

(define-for-cond-contract (fid-hashof c)
  (hash/c symbol? (listof (cons/c identifier? c))))


(define fid-hash
  (case-lambda
    [() #hash()]
    [l (let loop ([h #hash()]
                  [args l])
         (match args
           [(list) h]
           [(cons id (cons val rst))
            (loop (fid-hash-set h id val) rst)]
           [(list (? identifier? id))
            (error 'fid-hash
                   "identifier not provided a value: ~a"
                   (syntax-e id))]
           [_ (error 'fid-hash
                     "expected even length list of identifier/value bindings, given ~a"
                     (length l))]))]))

(define make-fid-hash
  (case-lambda
    [() (make-hash)]
    [l (define h (make-hash))
       (let loop ([args l])
         (match args
           [(list) h]
           [(cons id (cons val rst))
            (fid-hash-set! h id val)
            (loop rst)]
           [(list (? identifier? id))
            (error 'fid-hash
                   "identifier not provided a value: ~a"
                   (syntax-e id))]
           [_ (error 'fid-hash
                     "expected even length list of identifier/value bindings, given ~a"
                     (length l))]))
       h]))

(define-syntax fid-hash-empty? (make-rename-transformer #'hash-empty?))

(define (fid-hash-count h)
  (for/sum ([l (in-fid-hash-buckets h)])
    (length l)))


;; fid-hash-set
(define (fid-hash-set h x val)
  (define entry (cons x val))
  (define x-sym (identifier-binding-symbol x))
  (cond
    [(hash-ref h x-sym #f)
     => (λ (l)
          (hash-set h x-sym (cons entry (remf (λ (p) (free-identifier=? x (car p))) l))))]
    [else (hash-set h x-sym (list entry))]))

;; fid-hash-set!
(define (fid-hash-set! h x val)
  (define entry (cons x val))
  (define x-sym (identifier-binding-symbol x))
  (cond
    [(hash-ref h x-sym #f)
     => (λ (l)
          (hash-set! h x-sym (cons entry (remf (λ (p) (free-identifier=? x (car p))) l))))]
    [else (hash-set! h x-sym (list entry))]))


;; fid-hash-ref
(define (fid-hash-ref h x [fail (λ () (error 'fid-hash-ref
                                             "id not found in fid-hash: ~a"
                                             x))])
  (define (return-fail)
    (if (procedure? fail) (fail) fail))
  (cond
    [(hash-ref h (identifier-binding-symbol x) fail)
     => (λ (l)
          (match (assf (λ (y) (free-identifier=? x y)) l)
            [(cons _ val) val]
            [else (return-fail)]))]
    [else (return-fail)]))


;; fid-hash-has-key?
(define (fid-hash-has-key? h x)
  (define l (hash-ref h (identifier-binding-symbol x) #f))
  (and l (assf (λ (y) (free-identifier=? x y)) l) #t))


;; fid-hash-remove
(define (fid-hash-remove h x)
  (define x-sym (identifier-binding-symbol x))
  (cond
    [(hash-ref h x-sym #f)
     => (λ (l)
          (let ([l (remf (λ (p) (free-identifier=? x (car p))) l)])
            (if (null? l)
                (hash-remove h x-sym)
                (hash-set h x-sym l))))]
    [else h]))


;; fid-hash-remove
(define (fid-hash-remove! h x)
  (define x-sym (identifier-binding-symbol x))
  (cond
    [(hash-ref h x-sym #f)
     => (λ (l)
          (let ([l (remf (λ (p) (free-identifier=? x (car p))) l)])
            (if (null? l)
                (hash-remove! h x-sym)
                (hash-set! h x-sym l))))]
    [else (void)]))


(define (fid-hash-keys h)
  (for*/list ([b (in-fid-hash-buckets h)]
              [id/val (in-fid-hash-bucket b)])
    (car id/val)))

(define (fid-hash-values h)
  (for*/list ([b (in-fid-hash-buckets h)]
              [id/val (in-fid-hash-bucket b)])
    (cdr id/val)))

;; fid-hash iterating forms
(define-syntax in-fid-hash-buckets (make-rename-transformer #'in-hash-values))
(define-syntax in-fid-hash-bucket (make-rename-transformer #'in-list))


;; for/fid-hash
(define-syntax-rule (for/fid-hash bindings body ...)
  (for/fold ([h (fid-hash)])
            bindings
    (let-values ([(key val) (let () body ...)])
      (fid-hash-set h key val))))

;; for/mutable-fid-hash
(define-syntax-rule (for/mutable-fid-hash bindings body ...)
  (let ([h (make-fid-hash)])
    (for bindings
      (let-values ([(key val) (let () body ...)])
        (fid-hash-set! h key val)))
    h))


;; for*/fid-hash
(define-syntax-rule (for*/fid-hash bindings body ...)
  (for*/fold ([h (fid-hash)])
             bindings
    (let-values ([(key val) (let () body ...)])
      (fid-hash-set h key val))))

;; for*/mutable-fid-hash
(define-syntax-rule (for*/mutable-fid-hash bindings body ...)
  (let ([h (make-fid-hash)])
    (for* bindings
      (let-values ([(key val) (let () body ...)])
        (fid-hash-set! h key val)))
    h))

;; fid-copy
(define-syntax fid-hash-copy (make-rename-transformer #'hash-copy))


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
  (define h (fid-hash))

  (define-syntax (assert stx)
    (syntax-case stx ()
      [(_ expr)
       #`(unless expr #,(quasisyntax/loc stx (error 'assert "failed!")))]))

  ;; tests for immutable fid-hash features
  (let ()
    (assert (fid-hash-empty? h))
    (assert (= 0 (fid-hash-count h)))

    (set! h (fid-hash-set h x 0))
    (assert (not (fid-hash-empty? h)))
    (assert (= 1 (fid-hash-count h)))
    (assert (= (fid-hash-ref h x) 0))
    (assert (not (fid-hash-ref h x1 #f)))
    (assert (not (fid-hash-ref h x1 (λ () #f))))

    (set! h (fid-hash-set h x1 1))
    (set! h (fid-hash-set h x2 2))
    (set! h (fid-hash-set h y 'y))
    (assert (not (fid-hash-empty? h)))
    
    (assert (= 4 (fid-hash-count h)))
    (assert (= (fid-hash-ref h x) 0))
    (assert (= (fid-hash-ref h x1) 1))
    (assert (= (fid-hash-ref h x2) 2))
    (assert (eq? (fid-hash-ref h y) 'y))

    (set! h (fid-hash-remove h x2))
    (assert (not (fid-hash-empty? h)))
    (assert (= 3 (fid-hash-count h)))
    (assert (= (fid-hash-ref h x) 0))
    (assert (= (fid-hash-ref h x1) 1))
    (assert (eq? (fid-hash-ref h y) 'y))
  
    (set! h (for*/fid-hash ([b (in-fid-hash-buckets h)]
                            [id/val (in-fid-hash-bucket b)])
              (values (car id/val) (if (number? (cdr id/val))
                                       (add1 (cdr id/val))
                                       (cdr id/val)))))
  
    (assert (not (fid-hash-empty? h)))
    (assert (= 3 (fid-hash-count h)))
    (assert (= (fid-hash-ref h x) 1))
    (assert (= (fid-hash-ref h x1) 2))
    (assert (eq? (fid-hash-ref h y) 'y)))




  ;; tests for mutable fid-hash features
  (let ()
    (define h (make-fid-hash))
  
    (assert (fid-hash-empty? h))
    (assert (= 0 (fid-hash-count h)))

    (fid-hash-set! h x 0)
    (assert (not (fid-hash-empty? h)))
    (assert (= 1 (fid-hash-count h)))
    (assert (= (fid-hash-ref h x) 0))
    (assert (not (fid-hash-ref h x1 #f)))
    (assert (not (fid-hash-ref h x1 (λ () #f))))

    (fid-hash-set! h x1 1)
    (fid-hash-set! h x2 2)
    (fid-hash-set! h y 'y)
    (assert (not (fid-hash-empty? h)))
    
    (assert (= 4 (fid-hash-count h)))
    (assert (= (fid-hash-ref h x) 0))
    (assert (= (fid-hash-ref h x1) 1))
    (assert (= (fid-hash-ref h x2) 2))
    (assert (eq? (fid-hash-ref h y) 'y))

    (fid-hash-remove! h x2)
    (assert (not (fid-hash-empty? h)))
    (assert (= 3 (fid-hash-count h)))
    (assert (= (fid-hash-ref h x) 0))
    (assert (= (fid-hash-ref h x1) 1))
    (assert (eq? (fid-hash-ref h y) 'y))
  
    (set! h (for*/mutable-fid-hash ([b (in-fid-hash-buckets h)]
                                    [id/val (in-fid-hash-bucket b)])
              (values (car id/val) (if (number? (cdr id/val))
                                       (add1 (cdr id/val))
                                       (cdr id/val)))))

    (fid-hash-set! h y 42)
    (assert (not (fid-hash-empty? h)))
    (assert (= 3 (fid-hash-count h)))
    (assert (= (fid-hash-ref h x) 1))
    (assert (= (fid-hash-ref h x1) 2))
    (assert (eq? (fid-hash-ref h y) 42))))


