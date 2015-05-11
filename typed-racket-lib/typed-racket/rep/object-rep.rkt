#lang racket/base

;; Representation of "objects" --- these describe the
;; part of an environment that an expression accesses
;;
;; See "Logical Types for Untyped Languages" pg.3

(require "rep-utils.rkt" 
         "free-variance.rkt" 
         "filter-rep.rkt" 
         "../utils/utils.rkt" 
         (except-in racket/contract one-of/c)
         racket/match racket/dict racket/list racket/function
         (contract-req)
         (for-syntax racket/base)
         (utils tc-utils))

(provide object-equal?
         LExp?
         LExp-path-map
         LExp-const
         LExp-set-const
         LExp-coeffs
         LExp-coeff
         LExp-set-coeff
         LExp-paths
         LExp-minus
         LExp-plus
         LExp-add1
         LExp-simple?
         constant-LExp?
         LExp-scale
         LExp-multiply
         LExp-has-var?
         LExp->sexp
         LExp-gcd-shrink
         -lexp
         (rename-out [LExp:* LExp:]
                     [LExp-terms* LExp-terms]))

(def-pathelem CarPE () [#:fold-rhs #:base])
(def-pathelem CdrPE () [#:fold-rhs #:base])
(def-pathelem SyntaxPE () [#:fold-rhs #:base])
(def-pathelem ForcePE () [#:fold-rhs #:base])
;; t is always a Name (can't put that into the contract b/c of circularity)
(def-pathelem StructPE ([t Type?] [idx natural-number/c])
  [#:frees (λ (f) (f t))]
  [#:fold-rhs (*StructPE (type-rec-id t) idx)])
;; TODO(amk) add type so length can access lists
(def-pathelem LengthPE () [#:fold-rhs #:base])
(def-pathelem FieldPE () [#:fold-rhs #:base])

(def-object Empty () [#:fold-rhs #:base])
(define -empty-obj (*Empty))
(def-object Path ([p (listof PathElem?)] [v name-ref/c])
  [#:intern (list (map Rep-seq p) (hash-name v))]
  [#:frees (λ (f) (combine-frees (map f p)))]
  [#:fold-rhs (*Path (map pathelem-rec-id p) v)])


;; represents no info about the object of this expression
;; should only be used for parsing type annotations and expected types
(def-object NoObject () [#:fold-rhs #:base])

(define (object-equal? o1 o2) (= (Rep-seq o1) (Rep-seq o2)))

(define empty-hash (hash))


;; *****************************************************************************
;; Linear Expressions and related operations
(define-custom-hash-types path-hash
  #:key? Path?
  object-equal?
  Rep-seq)
(define empty-path-table (make-immutable-path-hash))

(def-object LExp ([const exact-integer?] 
                  [terms immutable-path-hash?])
  #:no-provide
  [#:intern (list const terms)]
  [#:frees (λ (f) (combine-frees (map f (dict-keys terms))))]
  [#:fold-rhs ;; warning - this returns Empty if any subterm is converted to Empty
   (internal-lexp-path-map object-rec-id const terms)])

;; LExp-path-map
;; applies f to each Path p in the terms
;; + forall p, if (f p) returns Empty for any p, Empty is returned
;; + forall p, if (f p) returns a LExp, it is multiplied by the
;;    coefficient of p and added to the LExp
;; + forall p, if (f p) = some Path, we just swap p and (f p) basically
(define/cond-contract (LExp-path-map f lexp)
  (-> (-> Path? Object?) LExp? 
      (or/c LExp? Empty?))
  (match lexp
    [(LExp: c ts) (internal-lexp-path-map f c ts)]
    [_ (int-err "invalid LExp for LExp-path-map: ~a" lexp)]))


(define/cond-contract (hash-scale-coeffs h scale)
  (-> immutable-path-hash? exact-integer? immutable-path-hash?)
  (for/fold ([h empty-path-table])
            ([p/c (in-dict-pairs h)])
    (match-define (cons p c) p/c)
    (hash-set-coeff h p (* scale c))))

(define/cond-contract (internal-lexp-path-map f const terms)
  (-> (-> Path? Object?) exact-integer? immutable-path-hash? 
      (or/c LExp? Empty?))
  (let loop ([term-pairs (dict->list terms)]
             [const* const]
             [terms* terms])
    (match term-pairs
      [(list) (*LExp const* terms*)]
      [(list (cons orig-p orig-c) rest ...)
       (match (f orig-p)
         [(and (Empty:) o) o]
         [(? Path? p*) 
          (cond 
            [(object-equal? orig-p p*)
             (loop rest const* terms*)]
            [else
             (define updated-terms 
               (hash-set-coeff (hash-set-coeff terms* orig-p 0) p* orig-c))
             (loop rest const* updated-terms)])]
         [(? LExp? l) 
          (match-define (LExp: c ts) l)
          (let-values ([(c* ts*) (internal-lexp-plus (* c orig-c) (hash-scale-coeffs ts orig-c) 
                                                     const*       terms*)])
            (loop rest c* (hash-set-coeff ts* orig-p 0)))]
         [x (int-err "internal-lexp-path-map function produced invalid result: ~a" x)])]
      [y (int-err "invalid term-pairs: ~a" term-pairs)])))


;; internal setter for coefficients of Paths
;; doesn't let any elements with a coefficient of 0
;; take up space in the dict
(define/cond-contract (hash-set-coeff h path i)
  (-> immutable-path-hash? Path? exact-integer?
      immutable-path-hash?)
  (if (= 0 i)
      (dict-remove h path)
      (dict-set h path i)))

;; internal getter for coefficients, defaults to 0 if
;; not found (as expected mathematically)
(define/cond-contract (hash-get-coeff h path)
  (-> immutable-path-hash? Path? exact-integer?)
  (dict-ref h path 0))

;; constructor for LExps
(define/cond-contract (-lexp . terms)
  (->* () () #:rest (listof (or/c exact-integer? (list/c exact-integer? Path?))) 
      LExp?)
  (define coef car)
  (define var cdr)
  (let loop ([c 0]
             [h empty-path-table]
             [terms terms])
    (match terms
      [`() (*LExp c h)]
      [`((,a ,p) . ,terms*)
       (loop c 
             (hash-set-coeff h p (+ a (hash-get-coeff h p)))
             terms*)]
      [`(,a . ,terms*) 
       (loop (+ a c) h terms*)]
      [_ (int-err "invalid terms in list->Lexp ~a" terms)])))

(define/cond-contract (LExp-set-const l c)
  (-> LExp? exact-integer? LExp?)
  (*LExp c (LExp-terms l)))

;; returns the coefficients from this LExp
;; (e.g. (LExp-coeffs (3x + 4y + z + 42)) 
;;   produces '(1 3 4))
(define/cond-contract (LExp-coeffs l)
  (-> LExp? (listof exact-integer?))
  (dict-values (LExp-terms l)))

;; returns the coefficients from this LExp
;; (e.g. (LExp-coeffs (3x + 4y + z + 42)) 
;;   produces '(1 3 4))
(define/cond-contract (LExp-coeff l p)
  (-> LExp? Path? exact-integer?)
  (hash-get-coeff (LExp-terms l) p))

(define/cond-contract (LExp-set-coeff l x cx)
  (-> LExp? Path? exact-integer? LExp?)
  (match l
    [(LExp: c terms) (*LExp c (hash-set-coeff terms x cx))]
    [_ (int-err "invalid lexp for LExp-set-coeff: ~a" l)]))

;; returns the terms from this LExp as cons pairs
;; of the form  (cons Variable Coefficient)
;; (e.g. (LExp-terms (3x + 4y + z + 42)) 
;;   produces '((x . 3) (y . 4) (z . 1)))
(define/cond-contract (LExp-terms* l)
  (-> LExp? (listof (cons/c Path? exact-integer?)))
  (dict->list (LExp-terms l)))

;; returns the Paths acting as the variables
;; in this linear expression
;; (e.g. (LExp-paths (3x + 4y + z + 42)) 
;;   produces '(x y z))
(define/cond-contract (LExp-paths l)
  (-> LExp? (listof Path?))
  (dict-keys (LExp-terms l)))


(define/cond-contract (internal-lexp-plus/minus c1 terms1 c2 terms2 plus?)
  (-> exact-integer? immutable-path-hash?
      exact-integer? immutable-path-hash?
      boolean?
      (values exact-integer? immutable-path-hash?))
  (define +/- (if plus? + -))
  (values (+/- c1 c2)
          (for/fold ([terms terms1])
                    ([p2 (in-list (dict-keys terms2))])
            (hash-set-coeff terms p2
                            (+/- (hash-get-coeff terms1 p2)
                                 (hash-get-coeff terms2 p2))))))
(define internal-lexp-plus (curryr internal-lexp-plus/minus #t))
(define internal-lexp-minus (curryr internal-lexp-plus/minus #f))


;; l1 - l2
(define/cond-contract (LExp-minus l1 l2)
  (-> LExp? LExp? LExp?)
  (match* (l1 l2)
    [((LExp: c1 terms1)
      (LExp: c2 terms2))
     (let-values ([(c terms) (internal-lexp-minus c1 terms1 c2 terms2)])
       (*LExp c terms))]
    [(_ _) (int-err "invalid LExp-minus arg(s): ~a ~a" l1 l2)]))


;; l1 + l2
(define/cond-contract (LExp-plus l1 l2)
  (-> LExp? LExp? LExp?)
  (match* (l1 l2)
    [((LExp: c1 terms1)
      (LExp: c2 terms2))
     (let-values ([(c terms) (internal-lexp-plus c1 terms1 c2 terms2)])
       (*LExp c terms))]
    [(_ _) (int-err "invalid LExp-minus arg(s): ~a ~a" l1 l2)]))


;; LExp-add1
(define/cond-contract (LExp-add1 l)
  (-> LExp? LExp?)
  (match l
    [(LExp: c terms) (*LExp (add1 c) terms)]
    [_ (int-err "invalid LExp-add1 argument: ~a" l)]))

;; constant-LExp?
;; returns #f if this LExp contains non-zero variables
;; else returns the constant value of the LExp
(define/cond-contract (constant-LExp? l)
  (-> LExp? (or/c #f exact-integer?))
  (match l
    [(LExp: c terms) (and (dict-empty? terms)
                          c)]
    [_ (int-err "invalid constant-LExp? argument: ~a" l)]))


;; LExp-scale
;; multiplies all scalars in the LExp l by n
(define/cond-contract (LExp-scale l n)
  (-> LExp? exact-integer? LExp?)
  (match l
    [(LExp: c terms)
     (cond 
       [(zero? n)
        (*LExp 0 empty-path-table)]
       [(= 1 n) l]
       [else
        (*LExp (* n c) (hash-scale-coeffs terms n))])]
    [_ (int-err "invalid LExp-scale lexp: ~a" l)]))

;; LExp-shrink
;; multiplies all scalars in the LExp l by 1/d
(define/cond-contract (LExp-gcd-shrink l1 l2)
  (-> LExp? LExp? (values LExp? LExp?))
  
  (define (scale-terms scale terms)
    (for/fold ([h empty-path-table])
              ([p/c (in-dict-pairs terms)])
      (match p/c
        [(cons p c) (hash-set-coeff h p (* scale c))]
        [_ (int-err "invalid path/const pair in LExp-gcd-shrink: ~a" p/c)])))
  
  (match* (l1 l2)
    [((LExp: c1 terms1) (LExp: c2 terms2))
     (define lexp-gcd (apply gcd (filter-not zero? (append (list c1 c2)
                                                           (dict-values terms1)
                                                           (dict-values terms2)))))
     (cond
       [(= 0 lexp-gcd) (values (*LExp 0 empty-path-table)
                               (*LExp 0 empty-path-table))]
       [(= 1 lexp-gcd) (values l1 l2)]
       [else
        (define n (/ lexp-gcd))
        (values (*LExp (* n c1) (scale-terms n terms1))
                (*LExp (* n c2) (scale-terms n terms2)))])]
    [(_ _) (int-err "invalid LExp(s) in LExp-gcd-shrink: ~a ~a" l1 l2)]))

(define/cond-contract (LExp-has-var? l x)
  (-> LExp? Path? boolean?)
  (not (zero? (hash-get-coeff (LExp-terms l) x))))

(define-match-expander LExp:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ c ps/cs) 
       #'(? LExp? (app (λ (l) (list (LExp-const l)
                                    (LExp-terms* l))) 
                       (list c ps/cs)))])))

;; l1 * l2
;; however, if the product is not a LExp
;; then Empty is returned
(define/cond-contract (LExp-multiply l1 l2)
  (-> LExp? LExp? (or/c LExp? Empty?))
  (cond
    [(constant-LExp? l1) 
     => (λ (const) (LExp-scale l2 const))]
    [(constant-LExp? l2)
     => (λ (const) (LExp-scale l1 const))]
    [else -empty-obj]))

; lexp-simple?
; IF the lexp (exp) contains only 1 variable and its coefficient
; is 1 and furthermore (= 0 (lexp-const exp)) then that variable
; THEN that variable is returned
; ELSE it returns #f
(define/cond-contract (LExp-simple? exp)
  (-> LExp? (or/c #f Path?))
  (match (LExp-terms* exp)
    [(list (cons x 1)) 
     (and (zero? (LExp-const exp))
          x)]
    [_ #f]))

(define (LExp->sexp l Path->sexp)
  (match l
    [(LExp: c terms) 
     (cond
       [(dict-empty? terms) c]
       [else
        (define terms*
          (for/list ([x/c (in-dict-pairs terms)])
            (match x/c
              [(cons x coeff)
               `(* ,coeff ,(Path->sexp x))]
              [_ (int-err "invalid term/coeff pair in LExp->sexp ~a" x/c)])))
        (cons '+ (if (zero? c) terms* (cons c terms*)))])]
    [_ (int-err "invalid LExp in LExp->sexp ~a" l)]))
