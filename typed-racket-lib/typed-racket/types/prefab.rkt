#lang racket/base

;; Utilities for dealing with prefab struct types

(require "../utils/utils.rkt"
         (contract-req)
         racket/list
         racket/match)

(provide/cond-contract [normalize-prefab-key
                        (-> prefab-key? integer? prefab-key?)]
                       [prefab-key->field-count
                        (-> prefab-key? integer?)]
                       [abbreviate-prefab-key
                        (-> prefab-key? prefab-key?)]
                       [prefab-key-subtype?
                        (-> prefab-key? prefab-key? any)]
                       [prefab-key->field-mutability
                        (-> prefab-key? (listof boolean?))])

;; Convert a prefab key to its expanded version
(define (normalize-prefab-key key field-length)
  (cond [(symbol? key) `(,key ,field-length (0 #f) #())]
        [(list? key)
         (define base-sym (car key))
         (define-values (base-clauses rst)
           (splitf-at (cdr key) (λ (x) (not (symbol? x)))))
         (define parent-fragments
           (let loop ([key rst] [fragments null])
             (cond [(null? key) fragments]
                   [else
                    (define-values (clauses rst)
                      (splitf-at (cdr key) (λ (x) (not (symbol? x)))))
                    (loop rst (cons (cons (car key) clauses)
                                    fragments))])))
         (define-values (processed-parents remaining-length)
           (for/fold ([processed null]
                      [field-length field-length])
                     ([parent (in-list parent-fragments)])
             (match parent
               [(list _ n (and auto (list auto-n _)) _)
                (values (cons parent processed)
                        (- field-length n auto-n))]
               [(list sym (? number? n) (and auto (list auto-n _)))
                (values (cons `(,sym ,n ,auto #()) processed)
                        (- field-length n auto-n))]
               [(list sym (? number? n) (? vector? mut))
                (values (cons `(,sym ,n (0 #f) ,mut) processed)
                        (- field-length n))]
               [(list sym n)
                (values (cons `(,sym ,n (0 #f) #()) processed)
                        (- field-length n))])))
         (define processed-base
           (match base-clauses
             [(list n _ _) (cons base-sym base-clauses)]
             [(list (? number? n) (and auto (list auto-n _)))
              `(,base-sym ,n ,auto #())]
             [(list (? number? n) (? vector? mut))
              `(,base-sym ,n (0 #f) ,mut)]
             [(list (and auto (list auto-n _)) (? vector? mut))
              `(,base-sym ,(- remaining-length auto-n) ,auto ,mut)]
             [(list (? number? n))
              `(,base-sym ,n (0 #f) #())]
             [(list (and auto (list auto-n _)))
              `(,base-sym ,(- remaining-length auto-n) ,auto #())]
             [(list (? vector? mut))
              `(,base-sym ,remaining-length (0 #f) ,mut)]
             [(list)
              `(,base-sym ,remaining-length (0 #f) #())]))
         (append processed-base (apply append processed-parents))]))

;; Accepts a normalized prefab key and returns the number of fields
;; a struct with this key should have
(define (prefab-key->field-count key)
  (let loop ([key key] [count 0])
    (cond [(null? key) count]
          [else
           (match-define (list _ len (list auto-len _) _ rst ...) key)
           (loop rst (+ len auto-len count))])))

;; Convert a prefab key to a shortened version
(define (abbreviate-prefab-key key)
  (let loop ([key key] [first? #t])
    (cond [(null? key) null]
          [(symbol? key) key]
          [(list? key)
           (define sym (car key))
           (define-values (other-clauses rst)
             (splitf-at (cdr key) (λ (x) (not (symbol? x)))))
           (define simplified-clauses
             (for/list ([elem (in-list other-clauses)]
                        #:unless (and first? (number? elem))
                        #:unless (and (list? elem)
                                      (= (car elem) 0))
                        #:unless (and (vector? elem)
                                      (= (vector-length elem) 0)))
               elem))
           (if (and (null? simplified-clauses)
                    (null? rst))
               sym
               (cons sym (append simplified-clauses
                                 (loop rst #f))))])))

;; Determine if the first prefab key can be a subtype of the second
;; Invariant: the keys are fully expanded (normalized)
(define (prefab-key-subtype? key1 key2)
  (or (equal? key1 key2)
      (suffix? key2 key1)))

(define (suffix? l1 l2)
  (for/or ([n (in-range (add1 (length l2)))])
    (equal? (drop l2 n) l1)))

;; Returns a list of flags indicating the mutability of prefab struct types
;; in order from parent to the children (#t is mutable, #f is not)
;; Precondition: the key is fully expanded
(define (prefab-key->field-mutability key)
  (let loop ([key key])
    (cond [(null? key) null]
          [else
           (match-define (list sym len auto mut parents ...) key)
           (define mut-list (vector->list mut))
           (append (loop parents)
                   (for/list ([idx (in-range len)])
                     (and (member idx mut-list) #t)))])))
