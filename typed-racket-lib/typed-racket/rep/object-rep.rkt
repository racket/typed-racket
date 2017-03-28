#lang racket/base

;; Representation of "objects" --- these describe the
;; part of an environment that an expression accesses
;;
;; See "Logical Types for Untyped Languages" pg.3

(require "../utils/utils.rkt"
         "fme-utils.rkt"
         racket/match
         "rep-utils.rkt"
         "core-rep.rkt"
         "free-variance.rkt"
         (env mvar-env)
         (contract-req))

(provide -id-path
         name-ref=?
         -path-elem-of
         -lexp-add1
         -lexp-sub1
         LExp-const
         constant-LExp?
         -lexp
         -obj+
         -obj*
         LExp?
         LExp-const
         LExp-terms
         LExp:
         genobj
         make-obj-seq
         obj-seq-next
         (rename-out [make-LExp* make-LExp]
                     [make-LExp raw-make-LExp])
         (all-from-out "fme-utils.rkt"))

(def-path-elem CarPE () [#:singleton -car])
(def-path-elem CdrPE () [#:singleton -cdr])
(def-path-elem SyntaxPE () [#:singleton -syntax-e])
(def-path-elem ForcePE () [#:singleton -force])
(def-path-elem FieldPE () [#:singleton -field])
(def-path-elem VecLenPE () [#:singleton -vec-len])
(def-path-elem StructPE ([t Type?] [idx natural-number/c])
  [#:frees (f) (f t)]
  [#:fmap (f) (make-StructPE (f t) idx)]
  [#:for-each (f) (f t)])


(define/match (-path-elem-of pe o)
  [(pe (Path: pes x)) (make-Path (cons pe pes) x)]
  [(_ _) -empty-obj])

(define/provide (-car-of o) (-path-elem-of -car o))
(define/provide (-cdr-of o) (-path-elem-of -cdr o))
(define/provide (-syntax-of o) (-path-elem-of -syntax-e o))
(define/provide (-force-of o) (-path-elem-of -force o))
(define/provide (-vec-len-of o) (-path-elem-of -vec-len o))
(define/provide (-struct-idx-of t idx o)
  (if (Empty? o) ;; lets not make the pe if we don't need to
      o
      (-path-elem-of (make-StructPE t idx) o)))

(def-object Path ([elems (listof PathElem?)] [name name-ref/c])
  [#:frees (f)  (combine-frees (map f elems))]
  [#:fmap (f) (make-Path (map f elems) name)]
  [#:for-each (f) (for-each f elems)]
  [#:custom-constructor
   (cond
     [(identifier? name)
      ;; we don't want objects for visibly mutated or top level variables
      (if (or (is-var-mutated? name)
              (and (not (identifier-binding name))
                   (not (local-tr-identifier? name))))
          -empty-obj
          (let ([name (normalize-id name)])
            (intern-double-ref!
             Path-intern-table
             name elems #:construct (make-Path elems name))))]
     [else (intern-double-ref!
            Path-intern-table
            name elems #:construct (make-Path elems name))])])

(define Path-intern-table (make-weak-hash))

(define (-id-path name) (make-Path null name))

;; generates a fresh id object
;; NOTE: use this wisely -- calling this function
;; all the time will consume memory leading to GC
;; time that may add up during typechecking
(define (genobj) (-id-path (genid)))

;; creates an "object sequence" -- use 'obj-seq-next'
;; to iterate through the sequence.
;; For an example use case, see subtype.rkt,
;; which uses a seq to reuse fresh objects for subtype
;; checking
(define (make-obj-seq)
  (for/fold ([seq (cons (genobj) (box #f))])
            ([_ (in-range 9)])
    (cons (genobj) seq)))

;; obj-seq-next
;;
;; returns 2 values
;; val 1 - the next object
;; val 2 - the rest of the sequence
(define (obj-seq-next s)
  (match s
    [(cons val vals)
     (values val vals)]
    [(box (cons val vals))
     (values val vals)]
    [(box #f)
     (define more (make-obj-seq))
     (if (box-cas! s #f more)
         (obj-seq-next more)
         (obj-seq-next s))]))

(def-object LExp ([const exact-integer?]
                  [terms (hash/c Path? (and/c exact-integer?
                                              (not/c zero?)))])
  #:no-provide
  [#:frees (f) (combine-frees (for/list ([x (in-terms-vars terms)]) (f x)))]
  [#:fmap (f)
   ;; applies f to each object p in the terms
   ;; + if, for any  p, (f p) returns Empty for any p, Empty is returned
   ;; + for any p where (f p) returns a LExp, it is multiplied by the
   ;;    original coefficient of p and added to the LExp
   ;; + for p's where (f p) = some Path, we just swap p and (f p) basically
   (define-values (new-const new-terms)
     (for*/fold ([c const]
                 [ts (make-terms)])
                ([orig-x (in-terms-vars terms)]
                 #:break (not c)
                 [x (in-value (f orig-x))])
       (match x
         ;; empty, this linear expression is kaputt
         [(Empty:) (values #f #f)]
         [(? Path? x) (values c (terms-set ts x (terms-ref terms orig-x)))]
         ;; a linear expression -- scale it by
         ;; the old path's coeff and add it
         [(LExp: new-const new-terms)
          (define old-coeff (terms-ref terms orig-x))
          (values (+ c (* old-coeff new-const))
                  (terms-add ts (terms-scale new-terms old-coeff)))])))
   (if new-const
       (make-LExp* new-const new-terms)
       ;; if const is #f then some term(s) became Empty
       -empty-obj)]
  [#:for-each (f) (for ([p (in-terms-vars terms)]) (f p))]
  [#:custom-constructor
   (intern-double-ref!
    LExp-intern-table
    terms const #:construct (make-LExp const terms))])

(define LExp-intern-table (make-weak-hash))


;; make-LExp* (provided as make-LExp)
;;
; IF the lexp (exp) contains only 1 variable,
;;   and that variables its coefficient is 1,
;    and the constant is 0
; THEN that lone variable is returned
; ELSE it returns the LExp
;; NOTE 1: We do this so there is a 'canonical form'
;; for linear expressions which are actually just
;; the underlying object, e.g. insteaf of
;; the LExp '0 + 1*x' we just want to return 'x'
;; NOTE 2: we will also provide 'make-LExp'
;; as 'raw-make-LExp' so that LeqProps can
;; have both the lhs and rhs be LExps regardless
;; if they are equivalent to a simpler object
(define/cond-contract (make-LExp* const terms)
  (-> exact-integer? hash? (or/c LExp? Path?))
  (cond
    [(and (eqv? 0 const)
          (eqv? 1 (terms-count terms))
          (for/fold ([res #f])
                    ([(obj coeff) (in-terms terms)])
            (and (eqv? 1 coeff) obj)))]
    [else (make-LExp const terms)]))


;; *****************************************************************************
;; Operations for Linear Expressions


;; constructor for LExps
(define/cond-contract (-lexp . raw-terms)
  (->* () () #:rest (listof (or/c exact-integer?
                                  name-ref/c
                                  Path?
                                  LExp?
                                  (list/c exact-integer? (or/c name-ref/c Path?))))
       (or/c LExp? Path?))
  (define-values (const terms)
    (for/fold ([c 0] [ts (make-terms)])
              ([term (in-list raw-terms)])
      (match term
        [(list (? exact-integer? coeff) (? Path? p))
         (values c (terms-set ts p (+ coeff (terms-ref ts p))))]
        [(list (? exact-integer? coeff) (? name-ref/c nm))
         (let ([p (-id-path nm)])
           (if (Empty? nm)
               (values c ts)
               (values c (terms-set ts p (+ coeff (terms-ref ts p))))))]
        [(? exact-integer? new-const)
         (values (+ new-const c) ts)]
        [(LExp: c* ts*)
         (values (+ c c*)
                 (for/fold ([ts ts])
                           ([(p coeff) (in-terms ts*)])
                   (terms-set ts p (+ coeff (terms-ref ts p)))))]
        [(? Object? p)
         (values c (terms-set ts p (add1 (terms-ref ts p))))]
        [(? name-ref/c var)
         (define p (-id-path var))
         (values c (terms-set ts p (add1 (terms-ref ts p))))])))
  (make-LExp* const terms))


;; LExp-add1
(define/cond-contract (-lexp-add1 l)
  (-> OptObject? OptObject?)
  (match l
    [(LExp: c terms)
     (make-LExp* (add1 c) terms)]
    [(? Object? p) (-lexp 1 p)]
    [(? Empty?) -empty-obj]))


;; LExp-add1
(define/cond-contract (-lexp-sub1 l)
  (-> OptObject? OptObject?)
  (match l
    [(LExp: c terms)
     (make-LExp* (sub1 c) terms)]
    [(? Object? p) (-lexp -1 p)]
    [(? Empty?) -empty-obj]))

;; constant-LExp?
;; returns #f if this LExp contains non-zero variables
;; else returns the constant value of the LExp
(define/cond-contract (constant-LExp? l)
  (-> OptObject? (or/c #f exact-integer?))
  (match l
    [(LExp: c terms)
     (if (hash-empty? terms)
         c
         #f)]
    [_ #f]))

(define/cond-contract (in-LExp? obj l)
  (-> Path? LExp? boolean?)
  (match l
    [(LExp: _ terms) (hash-has-key? terms obj)]))


;;******************************************************************************
;; Mathematical operations for Objects
(define/cond-contract (-obj* . objs)
  (->* () () #:rest (listof OptObject?) OptObject?)
  (match objs
    [(list) -empty-obj]
    [(list o) o]
    [(list o1 o2) (multiply-OptObjects o1 o2)]
    [(list o1 o2 os ...)
     (apply -obj* (multiply-OptObjects o1 o2) os)]))


;; multiply-Objects
;; 1. if either is empty, the result is empty
;; 2. if one is an object the other a constant-LExp?, then
;; the result is the object scaled by the constant
;; 3. if two non-constant objects are multiplied, the
;; result is empty (since we do not represent non-linear
;; expressions currently)
(define/cond-contract (multiply-OptObjects o1 o2)
  (-> OptObject? OptObject? OptObject?)
  (cond
    [(or (Empty? o1) (Empty? o2)) -empty-obj]
    [(and (LExp? o1) (constant-LExp? o1))
     => (scale-obj o2)]
    [(and (LExp? o2) (constant-LExp? o2))
     => (scale-obj o1)]
    [else -empty-obj]))

(define ((scale-obj o) c)
  (match o
    [(? Path?) (-lexp (list c o))]
    [(LExp: const terms)
     ;; scaling doesn't modify which objects are in the LExp! =)
     ;; just constants & coefficients
     (make-LExp* (* c const) (terms-scale terms c))]))



(define/cond-contract (-obj+ . objs)
  (->* () () #:rest (listof OptObject?) OptObject?)
  (match objs
    [(list) -empty-obj]
    [(list o) o]
    [(list o1 o2) (add-OptObjects o1 o2)]
    [(list o1 o2 os ...)
     (apply -obj+ (add-OptObjects o1 o2) os)]))

(define (add-OptObjects o1 o2)
  (match* (o1 o2)
    [(_ _) #:when (or (Empty? o1) (Empty? o2))
           -empty-obj]
    [((? Path?) (? Path?))
     (-lexp (list 1 o1) (list 1 o2))]
    [((? LExp? l) (? Path? p))
     (add-path-to-lexp p l)]
    [((? Path? p) (? LExp? l))
     (add-path-to-lexp p l)]
    [((LExp: c1 terms1) (LExp: c2 terms2))
     (make-LExp* (+ c1 c2) (terms-add terms1 terms2))]))

(define (add-path-to-lexp p l)
  (match l
    [(LExp: const terms)
     (make-LExp* const (terms-set terms p (add1 (terms-ref terms p))))]))
