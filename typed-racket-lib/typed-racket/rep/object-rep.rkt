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
         racket/match racket/dict
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
         (rename-out [LExp:* LExp:]
                     [list->LExp make-LExp]
                     [LExp-terms* LExp-terms]))

(def-pathelem CarPE () [#:fold-rhs #:base])
(def-pathelem CdrPE () [#:fold-rhs #:base])
(def-pathelem SyntaxPE () [#:fold-rhs #:base])
(def-pathelem ForcePE () [#:fold-rhs #:base])
;; t is always a Name (can't put that into the contract b/c of circularity)
(def-pathelem StructPE ([t Type?] [idx natural-number/c])
  [#:frees (λ (f) (f t))]
  [#:fold-rhs (*StructPE (type-rec-id t) idx)])
(def-pathelem LengthPE () [#:fold-rhs #:base])

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
   (LExp-path-map object-rec-id (*LExp const terms))])

;; LExp-path-map
;; applies f to each Path p in the terms
;; + forall p, if (f p) returns Empty for any p, Empty is returned
;; + forall p, if (f p) returns a LExp, it is multiplied by the
;;    coefficient of p and added to the LExp
;; + forall p, if (f p) = some Path, we just swap p and (f p) basically
(define/cond-contract (LExp-path-map f lexp)
  (-> (-> Path? Object?) LExp? 
      (or/c LExp? Empty?))
  (define terms (LExp-terms* lexp))
  (let/ec exit
    (for/fold ([lexp* lexp])
              ([p/c (in-dict-pairs terms)])
      (match-define (cons orig-p orig-c) p/c)
      (let ([new-o (f orig-p)])
        (match new-o
          [(Empty:) (exit new-o)]
          [(? Path? p*) 
           (if (object-equal? orig-p p*)
               lexp*
               (LExp-set-coeff (LExp-set-coeff lexp* orig-p 0) 
                               orig-p
                               0))]
          [(? LExp? l) (LExp-plus (LExp-set-coeff lexp* orig-p 0) 
                                  (LExp-scale l orig-c))])))))


;; internal setter for coefficients of Paths
;; doesn't let any elements with a coefficient of 0
;; take up space in the dict
(define/cond-contract (table-set-coeff h path i)
  (-> immutable-path-hash? Path? exact-integer?
      immutable-path-hash?)
  (if (= 0 i)
      (dict-remove h path)
      (dict-set h path i)))

;; internal getter for coefficients, defaults to 0 if
;; not found (as expected mathematically)
(define/cond-contract (table-get-coeff h path)
  (-> immutable-path-hash? Path? exact-integer?)
  (dict-ref h path 0))

;; constructor for LExps
(define/cond-contract (list->LExp terms)
  (-> (listof (or/c exact-integer? (list/c exact-integer? Path?))) 
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
             (table-set-coeff h p (+ a (table-get-coeff h p)))
             terms*)]
      [`(,a . ,terms*) 
       (loop (+ a c) h terms*)])))

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
  (table-get-coeff (LExp-terms l) p))

(define/cond-contract (LExp-set-coeff l x cx)
  (-> LExp? Path? exact-integer? LExp?)
  (match-let ([(LExp: c terms) l])
    (*LExp c (table-set-coeff terms x cx))))

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


;; LExp subtraction and addition helper
(define/cond-contract (terms-plus/minus terms1 terms2 plus?)
  (-> immutable-path-hash? immutable-path-hash? boolean?
      immutable-path-hash?)
  (define +/- (if plus? + -))
  (for/fold ([terms terms1])
            ([p2 (in-list (dict-keys terms2))])
    (define p2-const
      (+/- (table-get-coeff terms1 p2)
           (table-get-coeff terms2 p2)))
    (table-set-coeff terms p2 p2-const)))

;; l1 - l2
(define/cond-contract (LExp-minus l1 l2)
  (-> LExp? LExp? LExp?)
  (match-let ([(LExp: c1 terms1) l1]
              [(LExp: c2 terms2) l2])
    (*LExp (- c1 c2)
           (terms-plus/minus terms1 terms2 #f))))

;; l1 + l2
(define/cond-contract (LExp-plus l1 l2)
  (-> LExp? LExp? LExp?)
  (match-let ([(LExp: c1 terms1) l1]
              [(LExp: c2 terms2) l2])
    (*LExp (+ c1 c2)
           (terms-plus/minus terms1 terms2 #t))))


;; LExp-add1
(define/cond-contract (LExp-add1 l)
  (-> LExp? LExp?)
  (match-define (LExp: c terms) l)
  (*LExp (add1 c) terms))

;; constant-LExp?
;; returns #f if this LExp contains non-zero variables
;; else returns the constant value of the LExp
(define/cond-contract (constant-LExp? l)
  (-> LExp? (or/c #f exact-integer?))
  (match-define (LExp: c terms) l)
  (and (dict-empty? terms)
       c))


;; LExp-scale
;; multiplies all scalars in the LExp l by n
(define/cond-contract (LExp-scale l n)
  (-> LExp? exact-integer? LExp?)
  (match-define (LExp: c terms) l)
  (cond 
    [(zero? n)
     (*LExp 0 empty-path-table)]
    [(= 1 n) l]
    [else
     (*LExp (* n c)
            (for/fold ([h empty-path-table])
                      ([p/c (in-dict-pairs terms)])
              (match-define (cons p c) p/c)
              (table-set-coeff h p (* n c))))]))

;; LExp-shrink
;; multiplies all scalars in the LExp l by 1/d
(define/cond-contract (LExp-gcd-shrink l1 l2)
  (-> LExp? LExp? (values LExp? LExp?))
  
  (define (scale-terms scale terms)
    (for/fold ([h empty-path-table])
              ([p/c (in-dict-pairs terms)])
      (match-define (cons p c) p/c)
      (table-set-coeff h p (* scale c))))
  
  (match* (l1 l2)
    [((LExp: c1 terms1) (LExp: c2 terms2))
     (define lexp-gcd (apply gcd (append (list c1 c2)
                                         (dict-values terms1) 
                                         (dict-values terms2))))
     (cond
       [(= 1 lexp-gcd) (values l1 l2)]
       [else
        (define n (/ lexp-gcd))
        (values (*LExp (* n c1) (scale-terms n terms1))
                (*LExp (* n c2) (scale-terms n terms2)))])]
    [(_ _) (int-err "invalid LExp(s) in LExp-gcd-shrink: ~a ~a" l1 l2)]))

(define/cond-contract (LExp-has-var? l x)
  (-> LExp? Path? boolean?)
  (not (zero? (table-get-coeff (LExp-terms l) x))))

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
  (match-define (LExp: c terms) l)
  (cond
    [(dict-empty? terms) c]
    [else
     (define terms*
       (for/list ([x/c (in-dict-pairs terms)])
         (match-define (cons x coeff) x/c)
         (cond
           [(= 1 coeff) (Path->sexp x)]
           [else `(* ,coeff ,(Path->sexp x))])))
     (cons '+ (if (zero? c) terms* (cons c terms*)))]))
