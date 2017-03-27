#lang racket/base

(require "../utils/utils.rkt"
         (rep object-rep prop-rep fme-utils)
         (contract-req)
         racket/format
         racket/list
         (for-syntax racket/base syntax/parse)
         racket/match)

;; public API for reasoning about LeqProps
(provide/cond-contract
 [contradictory-Leqs?
  (-> LeqProp? LeqProp? boolean?)]
 [complementary-Leqs?
  (-> LeqProp? LeqProp? boolean?)]
 [Leq-implies-Leq?
  (-> LeqProp? LeqProp? boolean?)]
 [satisfiable-Leqs?
  (-> (listof LeqProp?) boolean?)]
 [Leqs-imply-Leq-or-not-Leq?
  (-> (listof LeqProp?) LeqProp? (or/c boolean? 'neither))]
 [Leqs-imply-Leq?
  (-> (listof LeqProp?) LeqProp? boolean?)]
 [Leqs-imply-not-Leq?
  (-> (listof LeqProp?) LeqProp? boolean?)]
 [Leqs-imply-Leqs?
  (-> (listof LeqProp?) (listof LeqProp?) boolean?)])


;; some internal contracts
(define-for-cond-contract lexp?
  (cons/c exact-integer?
          (and/c (hash/c any/c (and/c exact-integer?
                                      (not/c (=/c 0)))
                         #:immutable #t)
                 hash-eq?)))
(define-for-cond-contract leq? (cons/c lexp? lexp?))
(define-for-cond-contract sli? (listof leq?))


;; *****************************************************
;; public API functions


(define (contradictory-Leqs? l1 l2)
  (contradictory-leqs? (Leq->leq l1)
                       (Leq->leq l2)))

(define (complementary-Leqs? l1 l2)
  (contradictory-leqs? (leq-negate (Leq->leq l1))
                       (leq-negate (Leq->leq l2))))

(define (Leq-implies-Leq? l1 l2)
  (leq-imp-leq? (Leq->leq l1)
                (Leq->leq l2)))


(define (satisfiable-Leqs? leqs)
  (fme-sat? (Leqs->sli leqs)))

;; Leqs-imply-Leq-or-not-Leq?
;;
;; Determins if the Leqs in 'ls' either prove
;; or disprove 'l'
(define (Leqs-imply-Leq-or-not-Leq? ls l)
  (define leqs (Leqs->sli ls))
  (define ineq (Leq->leq l))
  (cond
    [(fme-imp-leq? leqs ineq) #t]
    [(fme-imp-leq? leqs (leq-negate ineq)) #f]
    [else 'neither]))


(define (Leqs-imply-Leq? ls l)
  (fme-imp-leq? (Leqs->sli ls) (Leq->leq l)))


(define (Leqs-imply-not-Leq? ls l)
  (fme-imp-leq? (Leqs->sli ls) (leq-negate (Leq->leq l))))


(define (Leqs-imply-Leqs? assumptions goals)
  (fme-imp? (Leqs->sli assumptions)
            (Leqs->sli goals)))


;; *****************************************************
;; helper functions


;; turns a list of Leqs into a sli (system of linear equations)
;; which is just a list of leqs (the internal version)
(define/cond-contract (Leqs->sli Leqs)
  (-> (listof LeqProp?) sli?)
  (map Leq->leq Leqs))

;; Leq to the internal leq rep
(define (Leq->leq l)
  (match l
    [(LeqProp: (LExp: c1 ts1) (LExp: c2 ts2))
     (leq (lexp c1 ts1) (lexp c2 ts2))]))


;; *****************************************************************************
;; Fourier-Motzkin elimination implementation
;; http://en.wikipedia.org/wiki/Fourier-Motzkin_elimination
;;
;; Uses non-matrix representations of data
;;
;; Decides satisfiability, implication, etc... of/between systems of linear inequalities
;; of the form ([(a_1x_1 + a_2x_2 + ... + a_c) <= (b_1x_1 + b_2x_2 + ... + b_c)] ...)
;; *****************************************************************************


(define-match-expander lexp:
  (lambda (stx)
    (syntax-case stx ()
      [(_ const exp)
       #'(cons const exp)])))

(define-match-expander leq:
  (lambda (stx)
    (syntax-case stx ()
      [(_ lhs rhs)
       #'(cons lhs rhs)])))

(define/cond-contract (lexp const terms)
  (-> exact-integer?
      (and/c (hash/c any/c (and/c exact-integer?
                                  (not/c (=/c 0)))
                     #:immutable #t)
             hash-eq?)
      lexp?)
  (cons const terms))

(define lexp-const car)
(define lexp-terms cdr)

(define-syntax-rule (lexp* t ...)
  (list->lexp (list t ...)))

(define/cond-contract (list->lexp terms)
  (-> (listof (or/c exact-integer? (list/c exact-integer? any/c))) lexp?)
  (let loop ([c 0]
             [h (make-terms)]
             [zxs terms])
    (match zxs
      [(list) (lexp c h)]
      [(cons (list coeff var) zxs-rst)
       (loop c (terms-set h var (+ coeff (terms-ref h var))) zxs-rst)]
      [(cons constant zxs-rst)
       (loop (+ constant c) h zxs-rst)])))

;; what is the coefficient of 'x' in 'l'
(define/cond-contract (lexp-coeff l x)
  (-> lexp? any/c exact-integer?)
  (terms-ref (lexp-terms l) x))


(module+ test
  (define-syntax (check-true stx)
    (syntax-case stx ()
      [(_ exp) (syntax/loc stx (unless (eq? #t exp) (error 'check-true "test failed!")))]))
  (define-syntax (check-false stx)
    (syntax-case stx ()
      [(_ exp) (syntax/loc stx (when (not (eq? #f exp)) (error 'check-false "test failed!")))]))
  (define-syntax (check-not-false stx)
    (syntax-case stx ()
      [(_ exp) (syntax/loc stx (unless exp (error 'check-not-false "test failed!")))]))
  (define-syntax (check-equal? stx)
    (syntax-case stx ()
      [(_ val exp)
       (syntax/loc stx (unless (equal? val exp)
                         (error 'check-equal? "not equal!")))]))

  (check-equal? (lexp* 1 '(1 x) '(42 y) 1) (lexp* '(42 y) 2 '(1 x)))
  (check-equal? (lexp* 0) (lexp* 0 '(0 x) '(0 y) '(0 z)))
  (check-equal? (lexp-coeff (lexp* 1 '(1 x) '(42 y)) 'y) 42)
  (check-equal? (lexp-const (lexp* 1 '(1 x) '(42 y))) 1)
  (check-equal? (lexp-coeff (lexp* '(1 x) '(42 y)) 'q) 0))

;; what variables are in this lexp
(define/cond-contract (lexp-vars exp)
  (-> lexp? list?)
  (terms-vars (lexp-terms exp)))

;; what scalars are in this lexp
(define/cond-contract (lexp-scalars exp)
  (-> lexp? (listof exact-integer?))
  (match exp
    [(lexp: c ts)
     (define coeffs (terms-coeffs ts))
     (if (zero? c) coeffs (cons c coeffs))]
    [_ (error 'lexp-scalars "given invalid lexp ~a" exp)]))

(module+ test
  (check-true (and (equal? (sort (lexp-vars (lexp* 17 '(42 x) '(2 z))) symbol<?)
                           (sort '(x z) symbol<?))
                   (= 2 (length (lexp-vars (lexp* 17 '(42 x) '(2 z)))))))
  (check-true (and (equal? (sort (lexp-scalars (lexp* 17 '(42 x) '(2 z))) <)
                           '(2 17 42))
                   (= 3 (length (lexp-scalars (lexp* 17 '(42 x) '(2 z))))))))


; lexp-scale
;; if multiplying any scalar by a results
;; in a non integer, error is thrown if
;; contracts are active
(define/cond-contract (lexp-scale exp a)
  (-> lexp? rational? lexp?)
  (cond
    [(eqv? a 0) (lexp 0 (make-terms))]
    [(= a 1) exp]
    [else
     (match exp
       [(lexp: c h)
        (lexp (* c a)
              (terms-scale h a))])]))

(module+ test
  (check-equal? (lexp-set (lexp* 17 '(42 x)) 'x 0)
                (lexp* 17)))
; lexp-set
; excludes items set to 0
(define/cond-contract (lexp-set exp x i)
  (-> lexp? any/c exact-integer? lexp?)
  (match exp
    [(lexp: c h)
     (lexp c
           (terms-set h x i))]))

; lexp-set-const
; sets the constant in an lexp
(define/cond-contract (lexp-set-const exp i)
  (-> lexp? exact-integer? lexp?)
  (lexp i
        (lexp-terms exp)))

(module+ test
  (check-equal? (lexp-set (lexp* 17) 'x 42)
                (lexp* 17 '(42 x)))
  (check-equal? (lexp-set (lexp* 17 '(2 x)) 'x 42)
                (lexp* 17 '(42 x)))
  (check-equal? (lexp-set-const (lexp* 17 '(2 x)) 42)
                (lexp* 42 '(2 x))))

; lexp-zero?
;; is this lexp equiv to 0?
(define/cond-contract (lexp-zero? exp)
  (-> lexp? boolean?)
  (match exp
    [(lexp: c h)
     (and (= 0 c) (terms-empty? h))]))

(module+ test
  (check-false (lexp-zero? (lexp* 17 '(42 x))))
  (check-false (lexp-zero? (lexp* 17)))
  (check-not-false (lexp-zero? (lexp* 0))))

; l1 - l2
(define/cond-contract (lexp-subtract l1 l2)
  (-> lexp? lexp? lexp?)
  (match* (l1 l2)
    [((lexp: c1 h1) (lexp: c2 h2))
     (lexp (- c1 c2)
           (terms-subtract h1 h2))]))

;; l1 + l2
(define/cond-contract (lexp-plus l1 l2)
  (-> lexp? lexp? lexp?)
  (match* (l1 l2)
    [((lexp: c1 h1) (lexp: c2 h2))
     (lexp (+ c1 c2)
           (terms-add h1 h2))]))

(module+ test
  (check-equal? (lexp-subtract (lexp* -1 '(2 x) '(3 y))
                            (lexp* -1 '(2 x) '(42 z)))
                (lexp* 0 '(3 y) '(-42 z)))
  (check-equal? (lexp-subtract (lexp* 0)
                            (lexp* -1 '(2 x) '(42 z)))
                (lexp* 1 '(-2 x) '(-42 z))))

; lexp-has-var?
; is 'x' a non-zero term in 'l'
(define/cond-contract (lexp-has-var? l x)
  (-> lexp? any/c boolean?)
  (not (zero? (lexp-coeff l x))))

(module+ test
  (check-false (lexp-has-var? (lexp* 17 '(42 x)) 'y))
  (check-not-false (lexp-has-var? (lexp* 17 '(42 x)) 'x)))


(define/cond-contract (lexp-add1 l)
  (-> lexp? lexp?)
  (match l
    [(lexp: c h)
     (lexp (add1 c) h)]))

(module+ test
  (check-equal? (lexp-add1 (lexp* 0)) (lexp* 1))
  (check-equal? (lexp-add1 (lexp* 1 '(5 x)))
                (lexp* 2 '(5 x))))

;; is this lexp just a constant? (i.e. no terms)
(define/cond-contract (constant-lexp? l)
  (-> lexp? (or/c exact-integer? #f))
  (match l
    [(lexp: const terms)
     (and (terms-empty? terms)
          const)]))

;; if this lexp is 1*x, return x
(define/cond-contract (simple-lexp? exp)
  (-> lexp? any/c)
  (match exp
    [(lexp: c terms)
     (and
      ;; ps is length 1? (i.e. only 1 variable)
      (and (zero? c)
           (= 1 (terms-count terms))
           (let ([pair (car (terms->list terms))])
             (and (= 1 (cdr pair))
                  (car pair)))))]))


(define/cond-contract (lexp->string e [pp #f])
  (-> lexp? (-> any/c any/c) string?)
  (define vars (terms-vars (lexp-vars e)))
  (define const (lexp-const e))
  (define term->string
    (λ (x) (string-append (if (= 1 (lexp-coeff e x))
                              ""
                              (number->string (lexp-coeff e x)))
                          "(" (if pp
                                  (pp x)
                                  (~a x)) ")")))
  (cond
    [(terms-empty? vars) (number->string const)]
    [(zero? const)
     (for/fold ([str (term->string (car vars))])
               ([var (in-list (cdr vars))])
       (string-append str " + " (term->string var)))]
    [else
     (for/fold ([str (number->string const)])
               ([var (in-list vars)])
       (string-append str " + " (term->string var)))]))


;; ********** ********** L E Q s ********* **********
(define/cond-contract (leq lhs rhs)
  (-> lexp? lexp? leq?)
  (cons lhs rhs))

(define leq-lhs car)
(define leq-rhs cdr)


(define/cond-contract (leq-lexps ineq)
  (-> leq? (values lexp? lexp?))
  (match ineq
    [(leq: lhs rhs)
     (values lhs rhs)]))



(define/cond-contract (leq-contains-var? ineq x)
  (-> leq? any/c boolean?)
  (match ineq
    [(leq: lhs rhs)
     (or (lexp-has-var? lhs x)
         (lexp-has-var? rhs x))]))

(define/cond-contract (leq-vars ineq)
  (-> leq? list)
  (let-values ([(l r) (leq-lexps ineq)])
    (remove-duplicates (append (lexp-vars l) (lexp-vars r)))))

; leq-negate
; ~ (l1 <= l2) ->
; l2 <= 1 + l1
; (obviously this is valid for integers only)
(define/cond-contract (leq-negate ineq)
  (-> leq? leq?)
  (match ineq
    [(leq: (lexp: c-l terms-l) (lexp: c-r terms-r))
     (define c-r* (add1 c-r))
     (cond
       [(eqv? c-l c-r*)
        (leq (lexp 0 terms-r) (lexp 0 terms-l))]
       [(< c-l c-r*)
        (leq (lexp (- c-r* c-l) terms-r) (lexp 0 terms-l))]
       [else
        (leq (lexp 0 terms-r) (lexp (- c-l c-r*) terms-l))])]))

(module+ test

  (check-equal? (leq-negate (leq (lexp* 0 '(1 x))
                                  (lexp* 0 '(1 y))))
                (leq (lexp* 1 '(1 y))
                     (lexp* 0 '(1 x)))))

;; leq-isolate-var
;; converts leq with x into either:
;;  1) ax <= by + cz + ...
;;  or
;;  2) by + cz + ... <= ax
;;  where a is a positive integer and x is on at most
;;  one side of the inequality
(define/cond-contract (leq-isolate-var ineq x)
  (-> leq? any/c leq?)
  (match ineq
    [(leq: (lexp: lhs-c lhs-ts) (lexp: rhs-c rhs-ts))
     (define x-lhs-coeff (terms-ref lhs-ts x))
     (define x-rhs-coeff (terms-ref rhs-ts x))
     (cond
       ;; ...1 + ax + ...2 <= ...3 + ax + ...4
       ;; remove x
       ;; --> ;; ...1 + ...2 <= ...3 + ...4
       [(eqv? x-lhs-coeff x-rhs-coeff)
        (define lhs* (lexp lhs-c (terms-remove lhs-ts x)))
        (define rhs* (lexp rhs-c (terms-remove rhs-ts x)))
        (leq lhs* rhs*)]
       ;; ...1 + ax + ...2 <= ...3 + bx + ...4  where a < b
       ;; isolate x so it is on the rhs
       ;; --> ...1 + ...2 - ...3 - ...4 <= (bx - ax)
       [(< x-lhs-coeff x-rhs-coeff)
        (define lhs* ;; lhs - rhs (w/ x removed from the result)
          (let ([lhs-c* (- lhs-c rhs-c)]
                [lhs-h* (terms-subtract lhs-ts rhs-ts)])
            (lexp lhs-c* (terms-remove lhs-h* x))))
        (define rhs* (lexp 0 (make-terms x (- x-rhs-coeff x-lhs-coeff))))
        (leq lhs* rhs*)]
       ;; ...1 + ax + ...2 <= ...3 + bx + ...4  where a > b
       ;; isolate x so it is on the lhs
       ;; --> (ax - bx) <= ...3 + ...4 - ...1 - ...2
       [else
        (define lhs* (lexp 0 (make-terms x (- x-lhs-coeff x-rhs-coeff))))
        (define rhs*
          (let ([rhs-c* (- rhs-c lhs-c)]
                [rhs-h* (terms-subtract rhs-ts lhs-ts)])
            (lexp rhs-c* (terms-remove rhs-h* x))))
        (leq lhs* rhs*)])]))

; x lhs
(module+ test
  (check-equal? (leq-isolate-var (leq (lexp* '(3 x) '(2 z) '(5 y))
                                       (lexp* '(1 x) '(1 z)))
                                 'x)
                (leq (lexp* '(2 x)) (lexp* '(-5 y) '(-1 z))))

  ;; x rhs
  (check-equal? (leq-isolate-var (leq (lexp* '(3 x) '(2 z) '(5 y))
                                       (lexp* '(1 z) '(33 x)))
                                 'x)
                (leq (lexp* '(1 z) '(5 y)) (lexp* '(30 x))))
  ;; x eq
  (check-equal? (leq-isolate-var (leq (lexp* '(42 x) '(2 z) '(5 y))
                                       (lexp* '(42 x) '(1 z)))
                                 'x)
                (leq (lexp* '(2 z) '(5 y))
                      (lexp* '(1 z))))
  ;; no x
  (check-equal? (leq-isolate-var (leq (lexp* '(2 z) '(5 y))
                                       (lexp* '(1 z)))
                                 'x)
                (leq (lexp* '(2 z) '(5 y))
                      (lexp* '(1 z))))

  ; x mix
  (check-equal? (leq-isolate-var (leq (lexp* '(2 x) '(4 y) 1)
                                       (lexp* '(2 y))) 'x)
                (leq (lexp* '(2 x))
                      (lexp* '-1 '(-2 y)))))


;; leq-join
;; takes a pair a1x <= l1 and l2 <= a2x
;; and returns a2l1 <= a1l2
(define/cond-contract (leq-join leq1 leq2 x)
  (-> leq? leq? any/c leq?)
  ;; leq1: ... + ax + .... <= ... + bx + ...
  ;; leq2: ... + cx + .... <= ... + dx + ...
  (let-values ([(l1 r1) (leq-lexps leq1)]
               [(l2 r2) (leq-lexps leq2)])
    (let ([a (lexp-coeff l1 x)] [b (lexp-coeff r1 x)]
          [c (lexp-coeff l2 x)] [d (lexp-coeff r2 x)])
      (cond
        ;; leq1: ax <= lexp1
        ;; leq2: lexp2 <= dx
        [(and (eqv? 0 b) (eqv? 0 c))
         (leq (lexp-scale l2 a)
              (lexp-scale r1 d))]
        ;; leq1: lexp1 <= bx
        ;; leq2: cx <= lexp2
        [(and (eqv? 0 a) (eqv? 0 d))
         (leq (lexp-scale l1 c)
              (lexp-scale r2 b))]
        [else
         (error 'leq-join "cannot join ~a and ~a by ~a" leq1 leq2 x)]))))

(module+ test
  (check-equal? (leq-join (leq (lexp* '(2 x))
                               (lexp* '(4 y) 10))
                          (leq (lexp* '(4 z) 2)
                               (lexp* '(4 x)))
                          'x)
                (leq (lexp* '(8 z) 4)
                     (lexp* '(16 y) 40))))


(define/cond-contract (leq-trivially-valid? ineq)
  (-> leq? boolean?)
  (let-values ([(l r) (leq-lexps ineq)])
      (or (and (constant-lexp? l)
               (constant-lexp? r)
               (<= (lexp-const l) (lexp-const r)))
          (equal? l r))))

(define/cond-contract (leq-trivially-invalid? ineq)
  (-> leq? boolean?)
  (let-values ([(l r) (leq-lexps ineq)])
    (and (constant-lexp? l)
         (constant-lexp? r)
         (< (lexp-const r) (lexp-const l)))))


(define/cond-contract (leq->string ineq [pp #f])
  (-> leq? (-> any/c any/c) string?)
  (define-values (lhs rhs) (leq-lexps ineq))
  (string-append (lexp->string lhs pp) " ≤ " (lexp->string rhs pp)))

;; ******** ******** SLIs ******** ********
;; (i.e. system of linear inequalities)

(define/cond-contract (sli->string sli)
  (-> sli? string?)
  (string-append
   (cond
     [(null? sli) "("]
     [else
      (for/fold ([str (leq->string (car sli))])
                ([ineq (cdr sli)])
        (string-append str " ∧ "(leq->string ineq)))])
   ")"))

(define/cond-contract (sli-trivially-valid? s)
  (-> sli? boolean?)
  (for/and ([ineq (in-list s)])
    (leq-trivially-valid? ineq)))

(define/cond-contract (sli-vars sli)
  (-> sli? list?)
  (remove-duplicates (append-map leq-vars sli)))

(module+ test

  (check-equal? (sort (sli-vars (list (leq (lexp* '(1 x))
                                           (lexp* '(1 y)))
                                      (leq (lexp* '(1 x) '(1 z))
                                           (lexp* '(1 q)))
                                      (leq (lexp* '(1 r) '(3 z))
                                           (lexp* '(1 x)))))
                      symbol<?)
                (sort (list 'r 'q 'z 'y 'x) symbol<?))
  (check-equal? (length (sli-vars (list (leq (lexp* '(1 x))
                                             (lexp* '(1 y)))
                                        (leq (lexp* '(1 x) '(1 z))
                                             (lexp* '(1 q)))
                                        (leq (lexp* '(1 r) '(3 z))
                                             (lexp* '(1 x))))))
                5))


;; sli-partition
;; partitions leq expressions into
;; 3 lists of x-normalized inequalities:
;;  value 1) set of (ax <= by + cz + ...) leqs
;;  value 2) set of form (by + cz + ... <= ax) leqs
;;  value 3) leqs w/o x
(define/cond-contract (sli-partition leqs x)
  (-> sli? any/c (values (listof leq?)
                         (listof leq?)
                         (listof leq?)))
  (for/fold ([xlhs '()]
             [xrhs '()]
             [nox '()])
            ([ineq (in-list leqs)])
    (let ([ineq (leq-isolate-var ineq x)])
      (cond
        [(lexp-has-var? (leq-lhs ineq) x)
         (values (cons ineq xlhs) xrhs nox)]
        [(lexp-has-var? (leq-rhs ineq) x)
         (values xlhs (cons ineq xrhs) nox)]
        [else
         (values xlhs xrhs (cons ineq nox))]))))

(module+ test
  (check-equal? (let-values ([(lt gt no)
                              (sli-partition (list (leq (lexp* '(2 x) '(4 y) 1)
                                                        (lexp* '(2 y))))
                                             'x)])
                  (list lt gt no))
                (list (list (leq (lexp* '(2 x))
                                 (lexp* '(-2 y) -1)))
                      (list)
                      (list)))
  (check-equal? (let-values ([(lt gt no)
                              (sli-partition (list (leq (lexp* '(2 x) '(4 y) 1)
                                                        (lexp* '(2 y)))
                                                   (leq (lexp* '(2 x) '(4 y))
                                                        (lexp* '(2 y) '(42 x))))
                                             'x)])
                  (list lt gt no))
                (list (list (leq (lexp* '(2 x))
                                 (lexp* '(-2 y) -1)))
                      (list (leq (lexp* '(2 y))
                                 (lexp* '(40 x))))
                      (list)))
  (check-equal? (let-values ([(lt gt no)
                              (sli-partition (list (leq (lexp* '(2 x) '(4 y) -1)
                                                        (lexp* '(2 y)))
                                                   (leq (lexp* '(2 x) '(4 y))
                                                        (lexp* '(2 y) '(42 x)))
                                                   (leq (lexp* '(2 z) '(4 y))
                                                        (lexp* '(2 y) '(42 q))))
                                             'x)])
                  (list lt gt no))
                (list (list (leq (lexp* '(2 x))
                                 (lexp* '(-2 y) 1)))
                      (list (leq (lexp* '(2 y))
                                 (lexp* '(40 x))))
                      (list (leq (lexp* '(2 z) '(4 y))
                                 (lexp* '(2 y) '(42 q)))))))

;; fme-elim
;; reduces the system of linear inequalties, eliminating x
;; (does so while preventing duplicate leqs)
(define/cond-contract (fme-elim sli x)
  (-> sli? any/c sli?)
  (define-values (xltleqs xgtleqs noxleqs) (sli-partition sli x))
  (define leqs (make-hash))
  (for ([l (in-list noxleqs)])
    (hash-set! leqs l #t))
  (for* ([l1 (in-list xltleqs)]
         [l2 (in-list xgtleqs)])
    (hash-set! leqs (leq-join l1 l2 x) #t))
  (hash-keys leqs))

;; sli-satisfiable?
(define/cond-contract (fme-sat? sli)
  (-> sli? boolean?)
  (let* ([vars (sli-vars sli)]
         [simple-system (for/fold ([s sli])
                                  ([x (in-list vars)])
                          (fme-elim s x))])
    (for/and ([ineq (in-list simple-system)])
      (leq-trivially-valid? ineq))))


(module+ test
  ; 3x + 2y <= 7; 6x + 4y <= 15;  -x <= 1; 0 <= 2y has integer solutions
  (check-true (fme-sat? (list (leq (lexp* '(3 x) '(2 y))
                                   (lexp* 7))
                              (leq (lexp* '(6 x) '(4 y))
                                   (lexp* 15))
                              (leq (lexp* '(-1 x))
                                   (lexp* 1))
                              (leq (lexp* 0)
                                   (lexp* '(2 y))))))

  ; 3x + 2y <= 4; 1 <= x; 1 <= y no solutions
  (check-false (fme-sat? (list (leq (lexp* '(3 x) '(2 y))
                                    (lexp* 4))
                               (leq (lexp* 1)
                                    (lexp* '(1 x)))
                               (leq (lexp* 1)
                                    (lexp* '(1 y)))))))

;;**********************************************************************
;; Logical Implication for Integer Linear Inequalities
;; using Fourier-Motzkin elimination
;;**********************************************************************

(define/cond-contract (fme-imp-leq? s ineq)
  (-> sli? leq? boolean?)
  (or (and (member ineq s) #t)
      (not (fme-sat? (cons (leq-negate ineq) s)))))

(module+ test
  ; transitivity! x <= y /\ y <= z --> x <= z
  (check-true (fme-imp-leq? (list (leq (lexp* '(1 x))
                                       (lexp* '(1 y)))
                                  (leq (lexp* '(1 y))
                                       (lexp* '(1 z))))
                            (leq (lexp* '(1 x))
                                 (lexp* '(1 z)))))


  ; x  <= x;
  (check-true (fme-imp-leq? (list)
                            (leq (lexp* '(1 x))
                                 (lexp* '(1 x)))))

  ; x  - 1 <= x + 1;
  (check-true (fme-imp-leq? (list)
                            (leq (lexp* '(1 x) -1)
                                 (lexp* '(1 x) 1))))


  ; x + y <= z; 1 <= y; 0 <= x --> x + 1 <= z
  (check-true (fme-imp-leq? (list (leq (lexp* '(1 x) '(1 y))
                                      (lexp* '(1 z)))
                                 (leq (lexp* 1)
                                      (lexp* '(1 y)))
                                 (leq (lexp*)
                                      (lexp* '(1 x))))
                            (leq (lexp* '(1 x) 1)
                                 (lexp* '(1 z))))))

;;**********************************************************************
;; Simple Inequality Implication   (does P imply Q)
;;**********************************************************************
(define/cond-contract (leq-imp-leq? P Q)
  (-> leq? leq? boolean?)
  (or (equal? P Q)
      ;; (P -> Q)  ==  (~P or Q) == ~(P and ~Q)
      (not (fme-sat? (list P (leq-negate Q))))))

(module+ test
  (check-true (leq-imp-leq? (leq (lexp* '(1 x))
                                 (lexp* '(1 y)))
                            (leq (lexp* '(1 x))
                                 (lexp* '(1 y)))))
  (check-true (leq-imp-leq? (leq (lexp* '(1 x))
                                 (lexp* 14))
                            (leq (lexp* '(1 x))
                                 (lexp* 15))))
  (check-true (leq-imp-leq? (leq (lexp* '(1 x) '(1 y))
                                 (lexp* 14))
                            (leq (lexp* '(1 x) '(1 y))
                                 (lexp* 20))))
  (check-false (leq-imp-leq? (leq (lexp* '(1 x) '(1 y))
                                  (lexp* 14))
                             (leq (lexp* '(1 x))
                                  (lexp* 14)))))

;;**********************************************************************
;; Contradictory leqs?  ~(P and Q)
;;**********************************************************************
(define/cond-contract (contradictory-leqs? P Q)
  (-> leq? leq? boolean?)
  ;; (P -> Q)  ==  (~P or Q) == ~(P and ~Q)
  (not (fme-sat? (list P Q))))

(module+ test
  (check-true (contradictory-leqs? (leq (lexp* 2)
                                        (lexp* 1))
                                   (leq (lexp* '(1 y))
                                        (lexp* '(1 x)))))
  (check-true (contradictory-leqs? (leq (lexp* '(1 x))
                                        (lexp* '(1 y)))
                                   (leq (lexp* 1 '(1 y))
                                        (lexp* '(1 x)))))
  (check-false (contradictory-leqs? (leq (lexp* '(1 x))
                                         (lexp* '(1 y)))
                                    (leq (lexp* '(1 x))
                                         (lexp* '(1 y)))))
  (check-false (contradictory-leqs? (leq (lexp* 1)
                                         (lexp* 2))
                                    (leq (lexp* 2)
                                         (lexp* 3)))))
;;**********************************************************************
;; Logical Implication for Systems of Integer Linear Inequalities
;; using Fourier-Motzkin elimination
;;**********************************************************************

(define/cond-contract (fme-imp? axioms goals)
  (-> sli? sli? boolean?)
  (for/and ([ineq (in-list goals)])
    (fme-imp-leq? axioms ineq)))


(module+ test
  ;; 4 <= 3 is false
  (check-false (fme-imp? (list)
                         (list (leq (lexp* 4)
                                    (lexp* 3)))))
  ;; P and ~P --> false
  (check-true (fme-imp? (list (leq (lexp*) (lexp* '(1 y)))
                              (leq-negate (leq (lexp*) (lexp* '(1 y)))))
                        (list (leq (lexp* 4)
                                   (lexp* 3)))))


  ;; x + y <= z; 0 <= y; 0 <= x --> x <= z /\ y <= z
  (check-true (fme-imp? (list (leq (lexp* '(1 x) '(1 y))
                                   (lexp* '(1 z)))
                              (leq (lexp*)
                                   (lexp* '(1 y)))
                              (leq (lexp*)
                                   (lexp* '(1 x))))
                        (list (leq (lexp* '(1 x))
                                   (lexp* '(1 z)))
                              (leq (lexp* '(1 y))
                                   (lexp* '(1 z))))))

  ;; x + y <= z; 0 <= y; 0 <= x -/-> x <= z /\ y <= q
  (check-false (fme-imp? (list (leq (lexp* '(1 x) '(1 y))
                                    (lexp* '(1 z)))
                               (leq (lexp*)
                                    (lexp* '(1 y)))
                               (leq (lexp*)
                                    (lexp* '(1 x))))
                         (list (leq (lexp* '(1 x))
                                    (lexp* '(1 z)))
                               (leq (lexp* '(1 y))
                                    (lexp* '(1 q))))))

  ;; 7x <= 29 --> x <= 4
  (check-true (fme-imp? (list (leq (lexp* '(7 x))
                                   (lexp* 29)))
                        (list (leq (lexp* '(1 x))
                                   (lexp* 4)))))
  ;; 7x <= 28 --> x <= 4
  (check-true (fme-imp? (list (leq (lexp* '(7 x))
                                   (lexp* 28)))
                        (list (leq (lexp* '(1 x))
                                   (lexp* 4)))))
  ;; 7x <= 28 does not --> x <= 3
  (check-false (fme-imp? (list (leq (lexp* '(7 x))
                                    (lexp* 28)))
                         (list (leq (lexp* '(1 x))
                                    (lexp* 3)))))


  ;; 7x <= 27 --> x <= 3
  (check-true (fme-imp? (list (leq (lexp* '(7 x))
                                   (lexp* 27)))
                        (list (leq (lexp* '(1 x))
                                   (lexp* 3)))))

  ;; 4x+3y+9z+20q-100r + 42 <= 4x+3y+9z+20q+100r;
  ;; x <= y + z;
  ;; 29r <= x + y + z + q;
  ;; 0 <= x;
  ;; 0 <= x + y + z;
  ;; 0 <= x + z;
  ;; x <= z
  ;; z + 1 <= t
  ;; 0 <= x + y;
  ;; 0 <= x + r;
  ;; 0 <= x + r + q;
  ;; -->
  ;; 0 <= t
  (check-true (fme-imp? (list (leq (lexp* '(4 x) '(3 y) '(9 z) '(20 q) '(-100 r) 42)
                                   (lexp* '(4 x) '(3 y) '(9 z) '(20 q) '(100 r)))
                              (leq (lexp* '(1 x))
                                   (lexp* '(1 y) '(1 z)))
                              (leq (lexp* '(29 r))
                                   (lexp* '(1 x) '(1 y) '(1 z) '(1 q)))
                              (leq (lexp*)
                                   (lexp* '(1 x)))
                              (leq (lexp*)
                                   (lexp* '(1 x) '(1 y) '(1 z)))
                              (leq (lexp*)
                                   (lexp* '(1 x) '(1 z)))
                              (leq (lexp* '(1 x))
                                   (lexp* '(1 z)))
                              (leq (lexp* '(1 z) 1)
                                   (lexp* '(1 t)))
                              (leq (lexp*)
                                   (lexp* '(1 x) '(1 y)))
                              (leq (lexp*)
                                   (lexp* '(1 x) '(1 r)))
                              (leq (lexp*)
                                   (lexp* '(1 x) '(1 r) '(1 q))))
                        (list (leq (lexp*)
                                   (lexp* '(1 t))))))

  ;; 4x+3y+9z+20q-100r + 42 <= 4x+3y+9z+20q+100r;
  ;; x <= y + z;
  ;; 29r <= x + y + z + q;
  ;; 0 <= x;
  ;; 0 <= x + y + z;
  ;; 0 <= x + z;
  ;; x <= z
  ;; z + 1 <= t
  ;; 0 <= x + y;
  ;; 0 <= x + r;
  ;; 0 <= x + r + q;
  ;; -/->
  ;; t <= 0
  (check-false (fme-imp? (list (leq (lexp* '(4 x) '(3 y) '(9 z) '(20 q) '(-100 r) 42)
                                    (lexp* '(4 x) '(3 y) '(9 z) '(20 q) '(100 r)))
                               (leq (lexp* '(1 x))
                                    (lexp* '(1 y) '(1 z)))
                               (leq (lexp* '(29 r))
                                    (lexp* '(1 x) '(1 y) '(1 z) '(1 q)))
                               (leq (lexp*)
                                    (lexp* '(1 x)))
                               (leq (lexp*)
                                    (lexp* '(1 x) '(1 y) '(1 z)))
                               (leq (lexp*)
                                    (lexp* '(1 x) '(1 z)))
                               (leq (lexp* '(1 x))
                                    (lexp* '(1 z)))
                               (leq (lexp* '(1 z) 1)
                                    (lexp* '(1 t)))
                               (leq (lexp*)
                                    (lexp* '(1 x) '(1 y)))
                               (leq (lexp*)
                                    (lexp* '(1 x) '(1 r)))
                               (leq (lexp*)
                                    (lexp* '(1 x) '(1 r) '(1 q))))
                         (list (leq (lexp* '(1 t))
                                    (lexp*))))))
