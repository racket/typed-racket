#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (types union subtype resolve utils)
         (only-in (types numeric-tower) -Int)
         racket/match racket/unsafe/ops)

(provide (rename-out [*remove remove]) overlap integer-overlap?)

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

;; tracks previously seen types for overlap,
;; prevents infinite recursion (similar to 
;; subtyping's approach)
(define memory (make-parameter null))

(define (remember t1-seq t2-seq A)
  (if (< t1-seq t2-seq)
      `((,t1-seq . ,t2-seq) . ,A)
      `((,t2-seq . ,t1-seq) . ,A)))

(define (seen? t1-seq t2-seq A)
  (if (< t1-seq t2-seq)
      (for/or ([pr (in-list A)])
        (and (eq? (unsafe-car pr) t1-seq)
             (eq? (unsafe-cdr pr) t2-seq)))
      (for/or ([pr (in-list A)])
        (and (eq? (unsafe-cdr pr) t1-seq)
             (eq? (unsafe-car pr) t2-seq)))))

;; this function may eventually prove to be not useful,
;; but for now it acts as a nice boolean for claims
;; that an LExp has some type t. If t overlaps with
;; Integer, fine, maybe it is of type t. If not,
;; boom! we know it's a contradiction
(define (integer-overlap? t)
  (overlap t -Int))

(define (overlap t1 t2)
  (overlap* (memory) t1 t2))

;; uses a store (A) to prevent infinite recursion (a la subtyping)
(define (overlap* A t1 t2)
  (let* ([t1s (unsafe-Rep-seq t1)]
         [t2s (unsafe-Rep-seq t2)]
         [ks (Type-key t1)]
         [kt (Type-key t2)]
         [A* (remember t1s t2s A)])
    (cond
      [(or (Bottom? t1) (Bottom? t2)) #f]
      [(type-equal? t1 t2) A*]
      [(seen? t1s t2s A) A]
      [(and (symbol? ks) (symbol? kt) (not (eq? ks kt))) #f]
      [(and (symbol? ks) (pair? kt) (not (memq ks kt))) #f]
      [(and (symbol? kt) (pair? ks) (not (memq kt ks))) #f]
      [(and (pair? ks) (pair? kt)
            (for/and ([i (in-list ks)]) (not (memq i kt))))
       #f]
      [else
       (match (list t1 t2)
         [(list-no-order (Univ:) _) A*]
         [(list-no-order (F: _) _) A*]
         [(list-no-order (Opaque: _) _) A*]
         [(list (Name/simple: n) (Name/simple: n*))
          (or (and (free-identifier=? n n*) A*)
              (overlap* A* (resolve-once t1) (resolve-once t2)))]
         [(list-no-order t (? Name? s))
          (overlap* A* t (resolve-once s))]
         [(list-no-order (? Mu? t) s) 
          (overlap* A* (unfold t) s)]
         [(list-no-order (Refinement: t _) s) (overlap* A* t s)]
         [(list-no-order (Union: ts) s)
          (let loop ([ts ts] [A* A*]) 
             (match ts
               [(list) #f]
               [(cons t* ts*)
                (or (overlap* A* t* s)
                    (loop ts* (remember (unsafe-Rep-seq t*) 
                                        (unsafe-Rep-seq s) 
                                        A*)))]))]
         [(list-no-order (? Poly?) _) A*] ;; conservative
         [(list (Base: s1 _ _ _) (Base: s2 _ _ _)) 
          (parameterize ([memory A*]) 
            (and (or (subtype t1 t2) (subtype t2 t1)) A*))]
         [(list-no-order (? Value? s) (? Base? t))
          (parameterize ([memory A*])
            (and (subtype s t) A*))] ;; conservative
         [(list (Syntax: t) (Syntax: t*)) 
          (overlap* A* t t*)]
         [(list-no-order (Syntax: _) _) 
          #f]
         [(list-no-order (Base: _ _ _ _) _) 
          #f]
         [(list-no-order (Value: (? pair?)) (Pair: _ _)) A*]
         [(list (Pair: a b) (Pair: a* b*)) 
          (let ([A* (overlap* A* a a*)])
            (and A* (overlap* A* b b*)))]
         ;; lots of things are sequences, but not values where sequence? produces #f
         [(list-no-order (Sequence: _) (Value: v)) 
          (and (sequence? v) A*)]
         [(list-no-order (Sequence: _) _) A*]
         ;; Values where evt? produces #f cannot be Evt
         [(list-no-order (Evt: _) (Value: v)) (and (evt? v) A*)]
         [(list-no-order (Pair: _ _) _) #f]
         [(list (Value: (? simple-datum? v1))
                (Value: (? simple-datum? v2)))
          (and (equal? v1 v2) A*)]
         [(list-no-order (Value: (? simple-datum?))
                         (or (? Struct?) (? StructTop?) (? Function?)))
          #f]
         [(list-no-order (Value: (not (? hash?)))
                         (or (? Hashtable?) (? HashtableTop?)))
          #f]
         [(list (Struct: n _ flds _ _ _)
                (Struct: n* _ flds* _ _ _)) 
          #:when (free-identifier=? n n*)
          (let loop ([flds flds] [flds* flds*] [A* A*]) 
            (match* (flds flds*)
              [((list) (list)) A*]
              [((cons f fs) (cons f* fs*))
               (match* (f f*)
                 [((fld: t _ _) (fld: t* _ _)) 
                  (let ([A* (overlap* A* t t*)])
                    (loop fs fs* A*))])]))]
         [(list (Struct: n #f _ _ _ _)
                (StructTop: (Struct: n* #f _ _ _ _))) 
          #:when (free-identifier=? n n*)
          A*]
         ;; n and n* must be different, so there's no overlap
         [(list (Struct: n #f flds _ _ _)
                (Struct: n* #f flds* _ _ _))
          #f]
         [(list (Struct: n #f flds _ _ _)
                (StructTop: (Struct: n* #f flds* _ _ _)))
          #f]
         [(list (and t1 (Struct: _ _ _ _ _ _))
                (and t2 (Struct: _ _ _ _ _ _)))
          (and (or (subtype t1 t2) (subtype t2 t1)) A*)]
         [else A*])])))


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
