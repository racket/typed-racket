#lang racket/base

;;TODO use contract-req
(require "../utils/utils.rkt"
         "rep-utils.rkt" 
         "free-variance.rkt" 
         racket/contract/base
         racket/match racket/dict
         racket/lazy-require racket/set
         racket/function
         (for-syntax racket/base)
         (utils tc-utils))

;; TODO use something other than lazy-require.
(lazy-require ["type-rep.rkt" (Type/c Univ? Bottom?)]
              ["object-rep.rkt" (Path?
                                 Empty?
                                 LExp?
                                 LExp-coeffs
                                 LExp-coeff
                                 LExp-set-coeff
                                 LExp-scale
                                 LExp-add1
                                 LExp-minus
                                 make-LExp
                                 constant-LExp?
                                 LExp-paths
                                 LExp-path-map
                                 LExp-has-var?
                                 LExp->sexp
                                 LExp-gcd-shrink
                                 object-equal?)]
              ["../types/filter-ops.rkt" (-or)])

(provide Filter/c FilterSet/c name-ref/c hash-name filter-equal?
         SLI?
         SLI-try-join
         SLI-satisfiable?
         SLI-trivially-valid?
         SLI-implies?
         SLIs-imply?
         SLI->LExp-pairs
         SLI-paths
         leq
         leq-negate
         add-SLI
         add-SLIs
         leqs->SLIs
         SLI-path-map
         SLI->sexp
         SLI-negate
         (rename-out [SLI:* SLI:]))

(define (Filter/c-predicate? e)
  (and (Filter? e) (not (NoFilter? e)) (not (FilterSet? e))))
(define Filter/c (flat-named-contract 'Filter Filter/c-predicate?))

(define FilterSet/c
  (flat-named-contract
   'FilterSet
   (λ (e) (or (FilterSet? e) (NoFilter? e)))))

;; A Name-Ref is any value that represents an object.
;; As an identifier, it represents a free variable in the environment
;; As a list, it represents a De Bruijn indexed bound variable
(define name-ref/c (or/c identifier? (list/c integer? integer?)))
(define (hash-name v) (if (identifier? v) (hash-id v) (list v)))

(define ((length>=/c len) l)
  (and (list? l)
       (>= (length l) len)))

(def-filter Bot () [#:fold-rhs #:base])
(def-filter Top () [#:fold-rhs #:base])

(define -bot (*Bot))
(define -top (*Top))

(def-filter TypeFilter ([t (and/c Type/c (not/c Univ?) (not/c Bottom?))] [p (or/c LExp? Path?)])
  [#:intern (list (Rep-seq t) (Rep-seq p))]
  [#:frees (λ (f) (combine-frees (map f (list t p))))]
  [#:fold-rhs (*TypeFilter (type-rec-id t) (object-rec-id p))])

(def-filter NotTypeFilter ([t (and/c Type/c (not/c Univ?) (not/c Bottom?))] [p (or/c LExp? Path?)])
  [#:intern (list (Rep-seq t) (Rep-seq p))]
  [#:frees (λ (f) (combine-frees (map f (list t p))))]
  [#:fold-rhs (*NotTypeFilter (type-rec-id t) (object-rec-id p))])

;; implication
(def-filter ImpFilter ([a Filter/c] [c Filter/c]))

(def-filter OrFilter ([fs (and/c (length>=/c 2)
                                 (listof (or/c TypeFilter? NotTypeFilter? ImpFilter?)))])
  [#:intern (map Rep-seq fs)]
  [#:fold-rhs (*OrFilter (map filter-rec-id fs))]
  [#:frees (λ (f) (combine-frees (map f fs)))])

(def-filter AndFilter ([fs (and/c (length>=/c 2)
                                  (listof (or/c OrFilter? TypeFilter? NotTypeFilter? ImpFilter?)))])
  [#:intern (map Rep-seq fs)]
  [#:fold-rhs (*AndFilter (map filter-rec-id fs))]
  [#:frees (λ (f) (combine-frees (map f fs)))])

(def-filter FilterSet ([thn Filter/c] [els Filter/c])
  [#:fold-rhs (*FilterSet (filter-rec-id thn) (filter-rec-id els))])

;; represents no info about the filters of this expression
;; should only be used for parsing type annotations and expected types
(def-filter NoFilter () [#:fold-rhs #:base])

(define (filter-equal? a b) (= (Rep-seq a) (Rep-seq b)))


(define-custom-hash-types path-hash
  #:key? Path?
  object-equal?
  Rep-seq)
;;******************************************************************************
;; Less-than-or-Equal-to internal propositions
;; (i.e. SLIs, which are Filters, use them internally,
;;  but they are not accessible outside of this file)
(define (leq l1 l2)
  (let-values ([(l1* l2*) (LExp-gcd-shrink l1 l2)])
    (cons l1* l2*)))

(define (leq? a)
  (match a
    [(cons lhs rhs) (and (LExp? lhs)
                         (LExp? rhs))]
    [_ #f]))

(define (leq-lhs x)
  (car x))

(define (leq-rhs x)
  (cdr x))

(define-match-expander leq:
  (lambda (stx)
    (syntax-case stx ()
      [(_ l1 l2)
       #'(cons l1 l2)])))

(define (leq-equal? leq1 leq2)
  (match* (leq1 leq2)
    [((leq: lt1 gt1) (leq: lt2 gt2))
     (and (object-equal? lt1 lt2)
          (object-equal? gt1 gt2))]
    [(_ _) (int-err "invalid arg(s) to leq-equal? ~a ~a" leq1 leq2)]))

(define (leq-hash l)
  (match l
    [(leq: lt gt)
     (bitwise-xor (Rep-seq lt) 
                  (Rep-seq gt))]
    [_ (int-err "cannot leq-hash ~a" l)]))

; leq-negate
; ~ (l1 <= l2) ->
; l2 <= 1 + l1 
; (obviously this is valid for integers only)
(define/cond-contract (leq-negate ineq)
  (-> leq? leq?)
  (match-define (leq: l r) ineq)
  (leq (LExp-add1 r) l))

;; leq-isolate-var
;; converts leq with x into either:
;;  1) ax <= by + cz + ...
;;  or
;;  2) by + cz + ... <= ax
;;  where a is a positive integer and x is on at most 
;;  one side of the inequality
(define/cond-contract (leq-isolate-var ineq p)
  (-> leq? Path? leq?)
  ;; ... + ax + .... <= ... + bx + ...
  (match ineq
    [(leq: l r)
     (define a (LExp-coeff l p))
     (define b (LExp-coeff r p))
     (cond
       [(= a b)
        (leq (LExp-set-coeff l p 0)
             (LExp-set-coeff r p 0))]
       [(< a b)
        (leq (LExp-set-coeff (LExp-minus l r) p 0)
             (make-LExp (list (list (- b a) p))))]
       [else
        (leq (make-LExp (list (list (- a b) p)))
             (LExp-set-coeff (LExp-minus r l) p 0))])]
    [_ (int-err "invalid leq? for leq-isolate-var" ineq)]))


;; leq-join
;; takes a pair a1x <= l1 and l2 <= a2x
;; and returns a2l1 <= a1l2
(define/cond-contract (leq-join leq1 leq2 x)
  (-> leq? leq? Path? leq?)
  ;; leq1: ... + ax + .... <= ... + bx + ...
  ;; leq2: ... + cx + .... <= ... + dx + ...
  (match* (leq1 leq2)
    [((leq: l1 r1) (leq: l2 r2))
     (match* ((LExp-coeff l1 x) 
              (LExp-coeff r1 x) 
              (LExp-coeff l2 x) 
              (LExp-coeff r2 x))
       ; leq1: ax <= l1, leq2: l2 <= dx
       [(a 0    0 d)
        (leq (LExp-scale l2 a)
             (LExp-scale r1 d))]
       ; leq1: l1 <= bx, leq2: cx <= l2
       [(0 b    c 0)
        (leq (LExp-scale l1 c)
             (LExp-scale r2 b))]
       [(_ _ _ _) 
        (int-err "cannot join ~a and ~a by ~a" leq1 leq2 x)])]
    [(_ _) (int-err "invalid leq(s) to leq-join: ~a ~a" leq1 leq2)]))


;; trivially-valid?
;; equal or integer inequalities
(define/cond-contract (leq-trivially-valid? ineq)
  (-> leq? boolean?)
  (match ineq
    [(leq: l r) (or (object-equal? l r)
                    (let ([l-val (constant-LExp? l)]
                          [r-val (constant-LExp? r)])
                      (and l-val r-val
                           (<= l-val r-val))))]
    [_ (int-err "invalid leq in leq-trivially-valid?" ineq)]))

(define-custom-set-types path-set
  #:elem? Path?
  object-equal?
  Rep-seq)
(define empty-path-set (make-immutable-path-set))

(define/cond-contract (leq-paths ineq)
  (-> leq? immutable-path-set?)
  (match ineq
    [(leq: l r)
     (define set-l
       (for/fold ([s empty-path-set])
                 ([p (in-list (LExp-paths l))])
         (set-add s p)))
     (define set-l+r
       (for/fold ([s set-l])
                 ([p (in-list (LExp-paths r))])
         (set-add s p)))
     set-l+r]
    [_ (int-err "invalid leq? to leq-paths: ~a" ineq)]))


(define-custom-set-types leq-set
  #:elem? leq?
  leq-equal?
  leq-hash)
(define empty-leq-set (make-immutable-leq-set))
(define empty-path-table (make-immutable-path-hash))

;; set of all paths (e.g. the variables) from the
;; internal set in an SLI
(define (internal-sli-path-set sys)
  (for/fold ([s empty-path-set])
            ([ineq (in-set sys)])
    (set-union s (leq-paths ineq))))

;;******************************************************************************
;; System of Linear Inequalities (and related ops)
(def-filter SLI ([system immutable-leq-set?] [paths immutable-path-set?])
  #:no-provide
  [#:intern system]
  [#:frees (λ (f) (combine-frees (set-map paths f)))]
  [#:fold-rhs (internal-sli-path-map object-rec-id system paths)])

(define-match-expander SLI:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ sli)
       #'(? SLI? sli)])))

(define/cond-contract (SLI-path-map f sli)
  (-> (-> Path? Object?) SLI? Filter?)
  (match sli
    [(SLI: sys paths)
     (internal-sli-path-map sys paths)]
    [_ (int-err "invalid SLI? to SLI-path-map: ~a" sli)]))

(define/cond-contract (internal-sli-path-map f system paths)
  (-> (-> Path? Object?) immutable-leq-set? immutable-path-set? 
      Filter?)
  ;; determine what paths are being mapped to Empty?
  ;; and which are translated to new Paths/LExps
  (define-values (new-path-map eliminated-paths)
    (for/fold ([pmap empty-path-table]
               [eliminated null])
              ([p (in-set paths)])
      (match (f p)
        [(? Empty?) 
         (values pmap (cons p eliminated))]
        [(or (? Path? o) (? LExp? o)) 
         (if (eq? p o)
             (values pmap eliminated)
             (values (dict-set pmap p o) eliminated))]
        [o (int-err "unknown object from function in SLI-map ~a" o)])))
  ;; perform FM-elimination for all paths that were mapped to Empty
  (define system-w/o-empties
    (for/fold ([sys system])
              ([p (in-list eliminated-paths)])
      (internal-sli-elim-path sys)))
  ;; define a function that can now go replace the surviving
  ;; paths with their appropriate values
  (define (path-fun p)
    (dict-ref new-path-map p p))
  ;; build the new system with the subst
  (define system*
    (for/fold ([sys system-w/o-empties])
              ([ineq (in-set system-w/o-empties)])
      (match ineq
        [(leq: lhs rhs)
         (define lhs* (LExp-path-map path-fun lhs))
         (define rhs* (LExp-path-map path-fun rhs))
         (if (and (eq? lhs lhs*) (eq? rhs rhs*))
             sys
             (set-add (set-remove sys ineq)
                      (leq lhs* rhs*)))])))
  ;; if our new system is trivial or contradictory,
  ;; return the appropriate filter,
  ;; otherwise just return the new SLI
  (cond
    [(internal-sli-trivially-valid? system*)
     -top]
    [(not (internal-sli-sat? system*))
     -bot]
    [else (*SLI system* (internal-sli-path-set system*))]))

(define empty-set (set))

(define (sets-intersect? set1 set2)
  (for/or ([p (in-set set1)])
    (set-member? set2 p)))

;; SLI-try-join
;; combine two SLIs if they share any paths
;; if they don't, return #f
(define/cond-contract (SLI-try-join s1 s2)
  (-> SLI? SLI? (or/c #f  SLI? Top? Bot?))
  (match* (s1 s2)
    [((SLI: sli1 ps1) (SLI: sli2 ps2))
     (cond 
       [(sets-intersect? sli1 sli2)
        (define system* (set-union sli1 sli2))
        (cond
          [(internal-sli-trivially-valid? system*)
           -top]
          [(not (internal-sli-sat? system*))
           -bot]
          [else (*SLI system* (set-union ps1 ps2))])]
       [else #f])]
    [(_ _) (int-err "invalid SLI(s) to SLI-try-join: ~a ~a" s1 s2)]))

;; takes a list of leqs and builds
;; the proper disjoint SLIs
(define/cond-contract (leqs->SLIs initial-leqs)
  (-> (listof leq?) (listof (or/c SLI? Top? Bot?)))
  
  ;; split up the leqs based on their overlap of paths
  (define/cond-contract (organize-leqs leqs)
    (-> (listof leq?) 
        (listof (cons/c immutable-leq-set? immutable-path-set?)))
    (for/fold ([SLI-list null])
              ([ineq (in-list leqs)])
      (define ineq-ps (leq-paths ineq))
      ;; build a list of (cons leq-set path-set) that will turn into
      ;; the SLIs after we're done sorting out who goes where
      (let loop ([SLIs SLI-list])
        (match SLIs
          ;; there are no SLIs yet, so make a new one
          [(list) (list (cons (set-add empty-leq-set ineq) ineq-ps))]
          ;; there is already in SLI
          [(cons (and S (cons sys p-set)) ss) 
           (cond
             ;; ineq ∩ S ≠ ∅ , so loop with S = (S ∪ {ineq}) more or less
             [(sets-intersect? p-set ineq-ps)
              (define s* (cons (set-add sys ineq) (set-union p-set ineq-ps)))
              (cons s* ss)]
             ;; ineq ∩ S = ∅, continue looking for an SLI with intersection
             [else
              (cons S (loop ss))])]))))
  
  ;; now actually build the SLIs
  (for/list ([sys/p-set (in-list (organize-leqs initial-leqs))])
    (match sys/p-set
      [(cons sys-set p-set)
       (cond
         [(internal-sli-trivially-valid? sys-set)
          -top]
         [(not (internal-sli-sat? sys-set))
          -bot]
         [else (*SLI sys-set p-set)])]
      [_ (int-err "invalid immutable-leq-set? immutable-path-set? pair from organize-leqs: ~a" sys/p-set)])))

;; internal-sli-partition
;; partitions leq expressions into
;; 3 lists of x-normalized inequalities:
;;  value 1) set of (ax <= by + cz + ...) leqs
;;  value 2) set of form (by + cz + ... <= ax) leqs
;;  value 3) leqs w/o x
(define/cond-contract (internal-sli-partition leqs x)
  (-> immutable-leq-set? Path? 
      (values immutable-leq-set? immutable-leq-set? immutable-leq-set?))
  (define leqs* (for/set ([ineq (in-set leqs)])
                  (leq-isolate-var ineq x)))
  (for/fold ([xlhs empty-leq-set]
             [xrhs empty-leq-set]
             [nox empty-leq-set])
            ([ineq (in-set leqs*)])
    (cond
      [(LExp-has-var? (leq-lhs ineq) x)
       (values (set-add xlhs ineq) xrhs nox)]
      [(LExp-has-var? (leq-rhs ineq) x)
       (values xlhs (set-add xrhs ineq) nox)]
      [else
       (values xlhs xrhs (set-add nox ineq))])))

;; cartesian-leq-set-map
;; map of f over each pair of cartesian
;; product of input sets
;; order not guaranteed
(define/cond-contract (cartesian-set-map f xs ys)
  (-> procedure? immutable-leq-set? immutable-leq-set? immutable-leq-set?)
  (for*/fold ([result (make-immutable-leq-set)]) 
             ([x (in-set xs)] 
              [y (in-set ys)])
    (set-add result (f x y))))

;; internal-sli-elim-path
;; reduces the system of linear inequalties,
;; removing x
(define/cond-contract (internal-sli-elim-path sli p)
  (-> immutable-leq-set? Path? immutable-leq-set?)
  (define-values (pltleqs pgtleqs nopleqs) 
    (internal-sli-partition sli p))
  (set-union (cartesian-set-map (curryr leq-join p) 
                                pltleqs
                                pgtleqs)
             nopleqs))

(define satisfiability-cache (make-hash))

;; sli-satisfiable?
(define/cond-contract (internal-sli-sat? sli)
  (-> immutable-leq-set? boolean?)
  (cond
    [(hash-has-key? satisfiability-cache sli)
     (hash-ref satisfiability-cache sli)]
    [else
     (define paths (internal-sli-path-set sli))
     ;; build a system where all variables are eliminated
     (define simplified-system
       (for/fold ([s sli]) 
                 ([p (in-set paths)])
         (internal-sli-elim-path s p)))
     ;; if all are trivially valid, then the system
     ;; is satisfiable
     (define result
       (for/and ([ineq (in-set simplified-system)])
         (leq-trivially-valid? ineq)))
     (hash-set! satisfiability-cache sli result)
     result]))

(define/cond-contract (SLI-satisfiable? sli)
  (-> SLI? boolean?)
  (internal-sli-sat? (SLI-system sli)))

(define/cond-contract (internal-sli-trivially-valid? sli)
  (-> immutable-leq-set? boolean?)
  (for/and ([ineq (in-set sli)])
    (leq-trivially-valid? ineq)))

(define/cond-contract (SLI-trivially-valid? sli)
  (-> SLI? boolean?)
  (internal-sli-trivially-valid? (SLI-system sli)))

;;**********************************************************************
;; Logical Implication for Integer Linear Inequalities
;; using Fourier-Motzkin elimination
;;**********************************************************************

(define/cond-contract (internal-sli-imp-leq? s ineq)
  (-> immutable-leq-set? leq? boolean?)
  (not (internal-sli-sat? (set-add s (leq-negate ineq)))))

;;**********************************************************************
;; Logical Implication for Systems of Integer Linear Inequalities
;; using Fourier-Motzkin elimination
;;**********************************************************************
(define sli-imp-cache (make-hash))

(define/cond-contract (internal-sli-imp? axioms goals)
  (-> immutable-leq-set? immutable-leq-set? 
      boolean?)
  (define proof-state (cons axioms goals))
  (cond
    [(hash-has-key? sli-imp-cache proof-state)
     (hash-ref sli-imp-cache proof-state)]
    [else
     (define result
       (for/and ([ineq (in-set goals)])
         (internal-sli-imp-leq? axioms ineq)))
     (hash-set! sli-imp-cache proof-state result)
     result]))

(define/cond-contract (SLI-implies? sli1 sli2)
  (-> SLI? SLI? boolean?)
  (internal-sli-imp? (SLI-system sli1) 
                     (SLI-system sli2)))

(define/cond-contract (SLIs-imply? slis goal)
  (-> (listof SLI?) SLI? boolean?)
  (for/or ([sli (in-list slis)])
    (SLI-implies? sli goal)))

(define (SLI->LExp-pairs s)
  (for/list ([ineq (in-set (SLI-system s))])
    (match ineq
      [(leq: lhs rhs) (cons lhs rhs)]
      [_ (int-err "invalid leq? given from SLI-system: ~a" ineq)])))


(define/cond-contract (add-SLI sli slis)
  (-> SLI? (listof SLI?) (or/c Bot? (listof SLI?)))
  (match slis
    [null (list sli)]
    [(cons sli* slis*)
     (match (SLI-try-join sli sli*)
       [#f (match (add-SLI sli slis*)
             [(? list? l) (cons sli* l)]
             [(? Bot? b) b])]
       [(? SLI? new-s) (cons new-s slis*)]
       [(? Top?) slis*]
       [(? Bot? b) b])]))

(define/cond-contract (add-SLIs new-slis slis)
  (-> (listof SLI?) (listof SLI?) (or/c Bot? (listof SLI?)))
  (for/fold ([accumulation slis])
            ([new-sli (in-list new-slis)])
    #:break (Bot? accumulation)
    (add-SLI new-sli accumulation)))

(define (SLI->sexp s Path->sexp)
  (match s
    [(SLI: sys _)
     (for/list ([lhs/rhs (in-set sys)])
       (match-define (leq: lhs rhs) lhs/rhs)
       `(,(LExp->sexp lhs Path->sexp) ≤ ,(LExp->sexp rhs Path->sexp)))]
    [_ (int-err "invalid SLI given to SLI->sexp: ~a" s)]))


(define/cond-contract (SLI-negate sli)
  (-> SLI? Filter?)
  (match sli
    [(SLI: sys ps)
     (apply -or (for/list ([ineq (in-set sys)])
                  (define sys* (make-immutable-leq-set (leq-negate ineq)))
                  (cond
                    [(internal-sli-trivially-valid? sys*)
                     -top]
                    [(not (internal-sli-sat? sys*))
                     -bot]
                    [else (*SLI sys* ps)])))]
    [_ (int-err "SLI-negate given invalid SLI?: ~a" sli)]))




