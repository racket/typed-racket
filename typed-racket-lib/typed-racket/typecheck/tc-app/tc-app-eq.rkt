#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse syntax/stx racket/match racket/unsafe/undefined
         (typecheck signatures tc-funapp)
         (types abbrev prop-ops utils match-expanders)
         (rep type-rep object-rep)

         (for-label racket/base racket/bool))

(import tc-expr^)
(export tc-app-eq^)

(define-literal-set eq-literals
  #:for-label
  (eq? equal? eqv? string=? symbol=? memq member memv))

;; comparators that inform the type system
;; `=' is not included. Its type is more useful than this typing rule.
(define-syntax-class comparator
  #:literal-sets (eq-literals)
  (pattern (~or eq? equal? eqv? string=? symbol=? member memq memv)))


(define-tc/app-syntax-class (tc/app-eq expected)
  (pattern (eq?:comparator v1 v2)
    ;; make sure the whole expression is type correct
    (match* ((tc/funapp #'eq? #'(v1 v2) (tc-expr/t #'eq?)
                        (stx-map single-value #'(v1 v2)) expected)
             ;; check thn and els with the eq? info
             (tc/eq #'eq? #'v1 #'v2))
      [((tc-result1: t) (tc-result1: t* f o))
           (ret t f o)])))


;; typecheck eq? applications
;; identifier expr expr -> tc-results
(define (tc/eq comparator v1 v2)
  (define (eq?-able e) (or (boolean? e) (keyword? e) (symbol? e) (eof-object? e) (eq? e unsafe-undefined)))
  (define (eqv?-able e) (or (eq?-able e) (number? e) (char? e)))
  (define (equal?-able e) #t)
  (define (id=? a b)
    (free-identifier=? a b #f (syntax-local-phase-level)))
  (define (ok? val)
    (define-syntax-rule (alt nm pred ...)
      (and (id=? #'nm comparator)
           (or (pred val) ...)))
    (or (alt symbol=? symbol?)
        (alt string=? string?)
        (alt eq? eq?-able)
        (alt eqv? eqv?-able)
        (alt equal? equal?-able)))
  (match* ((single-value v1) (single-value v2))
    [((tc-result1: (Val-able: (? ok? val1)) _ o1)
      (tc-result1: (Val-able: (? ok? val2)) _ o2))
     (ret -Boolean (-PS (-and (-is-type o1 (-val val2))
                              (-is-type o2 (-val val1)))
                        (-and (-not-type o1 (-val val2))
                              (-not-type o2 (-val val1)))))]
    [((tc-result1: t _ o) (tc-result1: (Val-able: (? ok? val))))
     (ret -Boolean (-PS (-is-type o (-val val)) (-not-type o (-val val))))]
    [((tc-result1: (Val-able: (? ok? val))) (tc-result1: t _ o))
     (ret -Boolean (-PS (-is-type o (-val val)) (-not-type o (-val val))))]
    [((tc-result1: t _ o)
      (or (and (? (lambda _ (id=? #'member comparator)))
               (tc-result1: (List: (list (and ts (Val-able: _)) ...))))
          (and (? (lambda _ (id=? #'memv comparator)))
               (tc-result1: (List: (list (and ts (Val-able: (? eqv?-able))) ...))))
          (and (? (lambda _ (id=? #'memq comparator)))
               (tc-result1: (List: (list (and ts (Val-able: (? eq?-able))) ...))))))
     (let ([ty (apply Un ts)])
       (ret (Un (-val #f) t)
            (-PS (-is-type o ty)
                 (-not-type o ty))))]
    [(_ _) (ret -Boolean)]))


