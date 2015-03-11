#lang racket/base

;;TODO use contract-req
(require "../utils/utils.rkt"
         "rep-utils.rkt" 
         "free-variance.rkt" 
         racket/contract/base
         racket/match
         racket/lazy-require racket/set
         (for-syntax racket/base)
         fme)

;; TODO use something other than lazy-require.
(lazy-require ["type-rep.rkt" (Type/c Univ? Bottom?)]
              ["object-rep.rkt" (Path? 
                                 LExp? 
                                 object-equal?
                                 stop-right-there-do-not-use-to-make-sli)])

(provide Filter/c FilterSet/c name-ref/c hash-name filter-equal?
         make-empty-SLI
         SLI-join
         SLI-implies?
         SLI-trivially-valid?
         complementary-SLIs?
         SLI->sexp
         (rename-out [SLI* make-SLI]
                     [SLI:* SLI:]))

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

;; System of Linear Inequalities
(def-filter SLI ([system sli?] [paths (listof Path?)])
  #:no-provide
  [#:intern system]
  [#:frees (λ (f) (combine-frees (map f paths)))]
  [#:fold-rhs 
   (let-values ([(paths* new-paths)
                 (for/fold ([paths* null]
                            [new-paths (hash)])
                           ([p (in-list paths)])
                   (let ([p* (object-rec-id p)])
                     (values (cons p* paths*)
                             (hash-set new-paths (Rep-seq p) (Rep-seq p*)))))])
     (*SLI (sli-var-map (λ (p) (hash-ref new-paths (Rep-seq p))) system)
           paths*))])


(define (make-empty-SLI)
  (*SLI (set) null))

;; smart constructor for SLIs
;; calls implementation in object-rep
;; that can peek at the gory details inside of LExps
(define (SLI* ineqs)
  ;; this is the one place it's okay to use
  ;; 'stop-right-there-do-not-use-make-sli' =)
  (let-values ([(sli ps) (stop-right-there-do-not-use-to-make-sli ineqs)])
    (cond 
      [(or (set-empty? sli)
           (sli-trivially-valid? sli)) -top]
      [(fme-sat? sli) (*SLI sli ps)]
      [else -bot])))


(define/cond-contract (SLI-join s1 s2)
  (-> SLI? SLI? SLI?)
  (match-let ([(SLI sli1 ps1) s1]
              [(SLI sli2 ps2) s2])
    (*SLI (set-union sli1 sli2)
          (union-Path-lists ps1 ps2))))

(define implication-cache (make-hash))

;; checks if one SLI implies another
;; note that this will only return true if 
;; the intersection of the paths in s1 and s2
;; is non-empty (i.e. an unsatisfiable s1
;; with no common paths in s2 will not
;; return #t)
(define/cond-contract (SLI-implies? s1 s2)
  (-> SLI? SLI? SLI?)
  (match-let ([(SLI sli1 ps1) s1]
              [(SLI sli2 ps2) s2])
    (cond
      [(hash-has-key? implication-cache (cons sl1 sl2)) 
       (hash-ref implication-cache (cons sl1 sl2))]
      ;; if the intersection of ps1 and ps2
      ;; is non-empty, call the FME solver
      [(for/or ([p1 (in-list ps1)])
        (memf (curry object-equal? p1) ps2))
       (let ([b (fme-imp? sli1 sli2)]) 
         (hash-set! implication-cache (cons sli1 sli2) b)
         b)]
      ;; we don't bother if there's no overlap
      [else 
       (hash-set! implication-cache (cons sli1 sli2) #f)
       #f])))

(define satisfiability-cache (make-hash))

(define/cond-contract (SLI-satisfiable? s)
  (-> SLI? boolean?)
  (match-let ([(SLI: sli _) s])
    (cond
      [(hash-has-key? satisfiability-cache sli)
       (hash-ref satisfiability-cache sli)]
      [else
       (let ([b (fme-sat? sli)])
         (hash-set! satisfiability-cache sli b)
         b)])))

(define/cond-contract (SLI-trivially-valid? s)
  (-> SLI? boolean?)
  (match-let ([(SLI s _)])
    (sli-trivially-valid? s)))

(define (SLI-elim-many s ps)
  (match-let ([(SLI: sli sli-ps) s])
    (let-values ([(sli* sli*-ps) 
                  (for/fold ([sli sli]
                             [sli-ps sli-ps])
                            ([p (in-list ps)])
                    (values (fme-elim sli (Rep-seq p))
                            (remove p sli-ps object-equal?)))])
      (cond
        [(sli-trivially-valid? sli*) -top]
        [else (*SLI sli* sli*-ps)]))))

(define complement-cache (make-hash))

(define/cond-contract (complementary-SLIs? sli1 sli2)
  (-> SLI? SLI? boolean?)
  (match-let ([(SLI: s1 _1) sli1]
              [(SLI: s2 _2) sli2])
    (cond
      [(hash-contains-key? complement-cache (cons s1 s2))
       (hash-ref complement-cache (cons s1 s2))]
      [(hash-contains-key? complement-cache (cons s2 s1))
       (hash-ref complement-cache (cons s2 s1))]
      [else
       (let ([b (complementary-slis? sli1 sli2)])
         (hash-set! complement-cache (cons sli1 sli2) b)
         b)])))

(define (SLI-elim s p)
  (SLI-elim s (list p)))

(define-match-expander SLI:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ ps) #'(? SLI? (app SLI-paths ps))])))

(define (SLI->sexp s ps->var->sexp)
  (match-let ([(SLI: sli ps) s])
    (sli->sexp sli (ps->var->sexp ps))))

;; used by substitution, performs necc operations
;; based on how the function affects each path in the SLI
;; (e.g. if it makes it Empty, then we perform FM-elim, etc)
(define/cond-contract (SLI-var-map s f)
  (-> SLI? (-> Path? Object?) Filter?)
  (match-define (SLI: sli ps) s)
  ;; apply f to all ps in the SLI
  ;; if (f p) =
  ;;  Empty  => we have to use FM-elim to remove p from the system
  ;;  Path   => its just a simple rename (possibly to itself)
  ;;  LExp   => we're going to have to substitute in this lexp
  ;;            for p and multiply it by the coefficient of p in each
  ;;            lexp p is found
  (define-values (sli* ps-ps* ps-lexp) 
    (for/fold ([sli* sli]
               [ps-ps* null]
               [ps-lexps null])
              ([p/p* (in-list (map (λ (p) (cons p (f p))) ps))])
      (match p/p*
        [(cons p (? Empty?))
         (values (SLI-elim p) ps-ps* ps-lexps)]
        [(cons p (? Path? p*))
         (values sli* (cons (cons p p*) ps-ps*) ps-lexps)]
        [(cons p (? LExp? l))
         (values sli* ps-ps* (cons (cons p l) ps-lexps))])))
  
(define (simple-rename p-seq)
    (match (assf (λ (p/p*) (equal? p-seq (Rep-seq (car p/p*)))) ps-ps*)
      [(cons p p*) (Rep-seq p*)]
      [_ p-seq]))
  
  (define sli** (sli-var-map sli* simple-rename))
  )


(def-filter FilterSet ([thn Filter/c] [els Filter/c])
  [#:fold-rhs (*FilterSet (filter-rec-id thn) (filter-rec-id els))])

;; represents no info about the filters of this expression
;; should only be used for parsing type annotations and expected types
(def-filter NoFilter () [#:fold-rhs #:base])

(define (filter-equal? a b) (= (Rep-seq a) (Rep-seq b)))









