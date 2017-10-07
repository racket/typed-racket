#lang typed/racket/base

(provide my-hash my-hash-set*)

(require racket/match)

(define-type (KV-List K V) (Rec T (U Null (List* K V T))))

(: my-hash (All (K V) (->* () #:rest-star (K V) (Immutable-HashTable K V))))
(define (my-hash . k/v-list)
  (let loop ([h : (Immutable-HashTable K V) (hash)]
             [to-add : (KV-List K V) k/v-list])
    (match to-add
      [(cons k (cons v rst)) 
       (loop (hash-set h k v) rst)]   
      [_ h])))

(: my-mutable-hash (All (K V) (->* () #:rest-star (K V) (Mutable-HashTable K V))))
(define (my-mutable-hash . k/v-list)
  (define h : (Mutable-HashTable K V) (make-hash))
  (let loop! ([to-add : (KV-List K V) k/v-list])
    (match to-add
      [(cons k (cons v rst))
       (hash-set! h k v)
       (loop! rst)]
      [_ h])))


(: my-hash-set* (All (K V) (->* ((Immutable-HashTable K V)) #:rest-star (K V) (Immutable-HashTable K V))))
(define (my-hash-set* orig . k/v-list)
  (let loop ([h : (Immutable-HashTable K V) orig]
             [to-add : (KV-List K V) k/v-list])
    (match to-add
      [(cons k (cons v rst))
       (loop (hash-set h k v) rst)]
      [_ h])))

(: my-hash-set*! (All (K V) (->* ((Mutable-HashTable K V)) #:rest-star (K V) Void)))
(define (my-hash-set*! orig . k/v-list)
  (let loop! ([to-add : (KV-List K V) k/v-list])
    (match to-add
      [(cons k (cons v rst))
       (hash-set! orig k v)
       (loop! rst)]
      [_ (void)])))


(define h (my-hash "Hello" 'world "How" 'are "you" 'today?))
(my-hash-set* h "one" 'more)

(define mh (my-mutable-hash "Hello" 'world "How" 'are "you" 'today?))
(my-hash-set*! mh "one" 'more)