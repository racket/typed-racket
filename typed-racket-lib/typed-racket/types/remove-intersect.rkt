#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (types union subtype resolve utils)
         racket/match)

(provide (rename-out [*remove remove]) overlap)

(define (simple-datum? v)
  (or (null? v)
      (symbol? v)
      (number? v)
      (boolean? v)
      (pair? v)
      (string? v)
      (keyword? v)
      (char? v)
      (void? v)
      (eof-object? v)))


(define (overlap t1 t2)
  (let ([ks (Type-key t1)] [kt (Type-key t2)])
    (cond
      [(type-equal? t1 t2) #t]
      [(and (symbol? ks) (symbol? kt) (not (eq? ks kt))) #f]
      [(and (symbol? ks) (pair? kt) (not (memq ks kt))) #f]
      [(and (symbol? kt) (pair? ks) (not (memq kt ks))) #f]
      [(and (pair? ks) (pair? kt)
            (for/and ([i (in-list ks)]) (not (memq i kt))))
       #f]
      [else
       (match (list t1 t2)
         [(list-no-order (Univ:) _) #t]
         [(list-no-order (F: _) _) #t]
         [(list-no-order (Opaque: _) _) #t]
         [(list (Name/simple: n) (Name/simple: n*))
          (or (free-identifier=? n n*)
              (overlap (resolve-once t1) (resolve-once t2)))]
         [(list-no-order t (? Name? s))
           (overlap t (resolve-once s))]
         [(list-no-order (? Mu? t) s) (overlap (unfold t) s)]
         [(list-no-order (Refinement: t _) s) (overlap t s)]
         [(list-no-order (Union: ts) s)
          (ormap (lambda (t*) (overlap t* s)) ts)]
         [(list-no-order (? Poly?) _) #t] ;; conservative
         [(list (Base: s1 _ _ _) (Base: s2 _ _ _)) (or (subtype t1 t2) (subtype t2 t1))]
         [(list-no-order (? Base? t) (? Value? s)) (subtype s t)] ;; conservative
         [(list (Syntax: t) (Syntax: t*)) (overlap t t*)]
         [(list-no-order (Syntax: _) _) #f]
         [(list-no-order (Base: _ _ _ _) _) #f]
         [(list-no-order (Value: (? pair?)) (Pair: _ _)) #t]
         [(list (Pair: a b) (Pair: a* b*)) (and (overlap a a*)
                                                (overlap b b*))]
         ;; lots of things are sequences, but not values where sequence? produces #f
         [(list-no-order (Sequence: _) (Value: v)) (sequence? v)]
         [(list-no-order (Sequence: _) _) #t]
         ;; Values where evt? produces #f cannot be Evt
         [(list-no-order (Evt: _) (Value: v)) (evt? v)]
         [(list-no-order (Pair: _ _) _) #f]
         [(list (Value: (? simple-datum? v1))
                (Value: (? simple-datum? v2)))
          (equal? v1 v2)]
         [(list-no-order (Value: (? simple-datum?))
                         (or (? Struct?) (? StructTop?) (? Function?)))
          #f]
         [(list-no-order (Value: (not (? hash?)))
                         (or (? Hashtable?) (? HashtableTop?)))
          #f]
         [(list (Struct: n _ flds _ _ _)
                (Struct: n* _ flds* _ _ _)) 
          #:when (free-identifier=? n n*)
          (for/and ([f (in-list flds)] [f* (in-list flds*)])
            (match* (f f*)
              [((fld: t _ _) (fld: t* _ _)) (overlap t t*)]))]
         [(list (Struct: n #f _ _ _ _)
                (StructTop: (Struct: n* #f _ _ _ _))) 
          #:when (free-identifier=? n n*)
          #t]
         ;; n and n* must be different, so there's no overlap
         [(list (Struct: n #f flds _ _ _)
                (Struct: n* #f flds* _ _ _))
          #f]
         [(list (Struct: n #f flds _ _ _)
                (StructTop: (Struct: n* #f flds* _ _ _)))
          #f]
         [(list (and t1 (Struct: _ _ _ _ _ _))
                (and t2 (Struct: _ _ _ _ _ _)))
          (or (subtype t1 t2) (subtype t2 t1))]
         [else #t])])))


;(trace overlap)

;; also not yet correct
;; produces old without the contents of rem
(define (*remove old rem)
  (define initial
    (if (subtype old rem)
        (Un) ;; the empty type
        (match (list old rem)
          [(list (or (App: _ _ _) (? Name?)) t)
           ;; must be different, since they're not subtypes
           ;; and n must refer to a distinct struct type
           old]
          [(list (Union: l) rem)
           (apply Un (map (lambda (e) (*remove e rem)) l))]
          [(list (? Mu? old) t) (*remove (unfold old) t)]
          [(list (Poly: vs b) t) (make-Poly vs (*remove b rem))]
          [_ old])))
  (if (subtype old initial) old initial))

;(trace *remove)
