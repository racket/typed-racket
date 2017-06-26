#lang racket/base

;; Test that immutable hash contracts run in low time/space

;; The Racket program at the bottom of this file runs very slowly
;;  without Immutable-HashTable types.
;; Reported by John Clements
;; https://groups.google.com/forum/#!searchin/racket-users/trie$20functions/racket-users/WBPCsdae5fs/J7CIOeV-CQAJ

(module trie typed/racket
  ;; Copied from pfds/trie, commit d4a8809b4d621dc3679d3473422a48d27b133a4f
  ;;  then changed "HashTable" to "Immutable-HashTable"

  (provide lookup bind trie insert Trie tries)

  (require scheme/match)
  (define-type-alias (Key A) (Listof A))

  (define-struct: Mt ())
  (define-struct: (A) Some ([elem : A]))

  (define-type-alias (Option A) (U Mt (Some A)))

  (define-struct: (K V) Trie ([opt : (Option V)]
                              [map : (Immutable-HashTable K (Trie K V))]))

  (: empty : (All (K V) (-> (Trie K V))))
  (define (empty) 
    (make-Trie (make-Mt) 
               (ann (make-immutable-hash null) (Immutable-HashTable K (Trie K V)))))

  (: lookup : (All (K V) ((Key K) (Trie K V) -> V)))
  (define (lookup keys map)
    (if (null? keys)
        (let ([opt (Trie-opt map)])
          (if (Mt? opt)
              (error 'lookup "given key not found in the trie")
              (Some-elem opt)))
        (let ([fst (car keys)]
              [hash (Trie-map map)])
          (with-handlers
              ([exn:fail? (lambda (error?) 
                            (error 'lookup "given key not found in the trie"))])
            (lookup (cdr keys) (hash-ref hash fst))))))

  (: bind : (All (K V) ((Key K) V (Trie K V) -> (Trie K V))))
  (define (bind lok v map)
    (let ([hash (Trie-map map)]
          [fst (car lok)]
          [rst (cdr lok)]
          [opt (Trie-opt map)])
      (make-Trie opt (hash-set hash fst 
                               (ann (with-handlers 
                                        ([exn:fail? 
                                          (lambda (error?) (build v rst))])
                                      (bind rst v (hash-ref hash fst)))
                                    (Trie K V))))))

  (: build : (All (K V) (V (Listof K) -> (Trie K V))))
  (define (build val lstk)
    (if (null? lstk)
        (make-Trie (make-Some val) 
                   (ann (make-immutable-hash null) 
                        (Immutable-HashTable K (Trie K V))))
        (make-Trie (make-Mt) 
                   (make-immutable-hash 
                    (list (cons (car lstk) (build val (cdr lstk))))))))

  (: trie : (All (K) ((Listof (Listof K)) -> (Trie K Integer))))
  (define (trie lst)
    (insert (get-vals lst) lst (ann (empty) (Trie K Integer))))

  (: get-vals : (All (K) ((Listof (Listof K)) -> (Listof Integer))))
  (define (get-vals lst)
    (: local : (All (K) (Integer (Listof (Listof K)) -> (Listof Integer))))
    (define (local ctr lstk)
      (if (null? (cdr lstk))
          (cons ctr null)
          (cons ctr (local (add1 ctr) (cdr lstk)))))
    (local 1 lst))

  ;; While creating the tree, 
  ;; if   (hash-ref hash k) throws an error, 
  ;; then it means that that there is no entry for k. So build a new
  ;;      Trie for rest of the key and create an entry for k. 
  ;; else go deeper into the insert searching for the rest of the key.

  (: insert : 
     (All (K V) ((Listof V) (Listof (Listof K)) (Trie K V) -> (Trie K V))))
  (define (insert lstv lstk tri)
    (match (list lstv lstk)
      [(list null null) tri]
      [(list (cons v vs) (cons (cons k ks) rstk))
       (let* ([hash (Trie-map tri)]
              [tree (ann (with-handlers ([exn:fail? (lambda (error?) 
                                                      (build v ks))])
                           (go-deep (hash-ref hash k) ks v)) 
                         (Trie K V))])
         (insert vs rstk
                 (make-Trie (Trie-opt tri) (hash-set hash k tree))))]))

  (: tries : (All (K V) ((Listof V) (Listof (Listof K)) -> (Trie K V))))
  (define (tries lstv lstk)
    (insert lstv lstk (ann (empty) (Trie K V))))

  ;; Uses the same trick as previous one does
  (: go-deep : (All (K V) ((Trie K V) (Listof K) V -> (Trie K V))))
  (define (go-deep tri lstk val)
    (if (null? lstk)
        (make-Trie (make-Some val) (Trie-map tri))
        (let* ([hash (Trie-map tri)]
               [k (car lstk)]
               [ks (cdr lstk)]
               [insert (ann (with-handlers
                                ([exn:fail? (lambda (error?) (build val ks))])
                              (go-deep (hash-ref hash k) ks val))
                            (Trie K V))])
          (make-Trie (Trie-opt tri) (hash-set hash k insert)))))
) (require 'trie racket/sandbox)

(define (main)
  (define (rand-list)
    (for/list ([i (in-range 128)])
        (random 256)))
  (define t (trie (list (rand-list))))
  (bind (rand-list) 0 t)
  (void))

(call-with-limits 2 3
  main)
