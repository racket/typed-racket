#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (utils tc-utils)
         "substitute.rkt" "tc-result.rkt" "tc-error.rkt"
         (except-in "base-abbrev.rkt" -> ->*)
         (rep free-variance) 
         racket/match
         racket/set
         racket/list
         (contract-req))


(provide (all-from-out "tc-result.rkt" "tc-error.rkt"))


(define dom+rst-ref-failure (λ () (int-err "invalid index for domain and rest args")))

;; given the list of domain types (dom)
;; and the functions rest spec (rst),
;; get the type for an argument at position idx,
;; else return default if no such type exists
;; where default is a procedure (i.e. a thunk
;; to be called in tail position)
;; or some other value (to be returned)
(define (dom+rst-ref dom rst idx [default dom+rst-ref-failure])
  (match dom
    [(cons t ts)
     (cond
       [(zero? idx) t]
       [else (dom+rst-ref ts rst (sub1 idx) default)])]
    [_ (match rst
         [(Rest: rst-ts) (list-ref rst-ts (remainder idx (length rst-ts)))]
         [_ (if (procedure? default) (default) default)])]))

(define (Rest->Type r)
  (match r
    [#f -Null]
    [(Rest: (list t)) (-lst t)]
    [(Rest: (list)) -Null]
    [(Rest: ts) (make-CyclicListof ts)]
    [(RestDots: dty dbound) (make-ListDots dty dbound)]))

(define (instantiate-poly t types)
  (match t
    [(Poly: ns body)
     (unless (<= (length types) (length ns))
       (int-err "instantiate-poly: wrong number of types: expected ~a, got ~a"
                (length ns) (length types)))
     ;; use Any as the default type for any omitted types
     (subst-all (make-simple-substitution ns (list-extend ns types Univ))
                body)]
    [(PolyDots: (list fixed ... dotted) body)
     (unless (>= (length types) (length fixed))
       (int-err
        "instantiate-poly: wrong number of types: expected at least ~a, got ~a"
        (length fixed) (length types)))
     (let* ([fixed-tys (take types (length fixed))]
            [rest-tys (drop types (length fixed))]
            [body* (subst-all (make-simple-substitution fixed fixed-tys)
                              body)])
       (substitute-dots rest-tys #f dotted body*))]
    [(PolyRow: names _ body)
     (unless (= (length types) (length names))
       (int-err "instantiate-poly: wrong number of types: expected ~a, got ~a"
                (length names) (length types)))
     (subst-all (make-simple-substitution names types) body)]
    [_ (int-err "instantiate-poly: requires Poly type, got ~a" t)]))

(define (instantiate-poly-dotted t types image var)
  (match t
    [(PolyDots: (list fixed ... dotted) body)
     (unless (= (length fixed) (length types))
       (int-err (string-append "instantiate-poly-dotted: wrong number of"
                               " types: expected ~a, got ~a, types were ~a")
                (length fixed) (length types) types))
     (let ([body* (subst-all (make-simple-substitution fixed types) body)])
       (substitute-dotted null image var dotted body*))]
    [_ (int-err "instantiate-poly-dotted: requires PolyDots type, got ~a" t)]))


;; fv : Type -> Listof[Symbol]
(define (fv t) (set->list (free-vars-names (free-vars* t))))
(define (fi t) (set->list (free-vars-names (free-idxs* t))))

;; fv/list : Listof[Type] -> Setof[Symbol]
(define (fv/list ts)
  (apply set-union (seteq) (map (compose free-vars-names free-vars*) ts)))

;; a parameter for the current polymorphic structure being defined
;; to allow us to prevent non-regular datatypes
(define-struct poly (name vars) #:prefab)
(define current-poly-struct (make-parameter #f))

;; UNUSED
;; a table indicating what variables should be abstracted away before using
;; this expected type keyed on the numeric Rep sequence
(define to-be-abstr
  (make-weak-hash))

(provide to-be-abstr)

;; has-optional-args? : (Listof arr) -> Boolean
;; Check if the given arrs meet the necessary conditions to be printed
;; with a ->* constructor or for generating a ->* contract
(define (has-optional-args? arrows)
  (and (pair? arrows)
       (pair? (cdr arrows)) ;; i.e. (> (length arrows) 1)
       (match arrows
         [(cons (Arrow: dom #f kws rng) as)
          (let loop ([dom dom]
                     [to-check (cdr arrows)])
            (match to-check
              [(cons (Arrow: next-dom next-rst next-kws next-rng)
                     remaining)
               (cond
                 ;; a #:rest must be the LAST clause,
                 ;; can't be a rest dots
                 [(and next-rst (or (not (null? remaining))
                                    (RestDots? next-rst)))
                  #f]
                 ;; keywords and range must be the same
                 [(not (and (equal? kws next-kws)
                            (equal? rng next-rng)))
                  #f]
                 [else
                  ;; next arrow should have one more domain type
                  ;; and their domains should be pointwise equal
                  ;; for all other positional args
                  (define dom-len (length dom))
                  (define next-dom-len (length next-dom))
                  (and (= next-dom-len (add1 dom-len))
                       (for/and ([d (in-list dom)]
                                 [next-d (in-list next-dom)])
                         (equal? d next-d))
                       (loop next-dom remaining))])]
              [_ #t]))]
         [_ #f])))

(provide/cond-contract
 [instantiate-poly ((or/c Poly? PolyDots? PolyRow?) (listof Rep?)
                    . -> . Rep?)]
 [instantiate-poly-dotted
  (PolyDots? (listof Rep?) Rep? symbol? . -> . Rep?)] 
 [fv (Rep? . -> . (listof symbol?))]
 [fi (Rep? . -> . (listof symbol?))]
 [fv/list ((listof Rep?) . -> . (set/c symbol?))]
 [current-poly-struct (parameter/c (or/c #f poly?))]
 [has-optional-args? (-> (listof Arrow?) any)]
 [Rest->Type (-> (or/c #f Rest? RestDots?) Type?)]
 [dom+rst-ref (->* ((listof Type?) (or/c #f Rest? RestDots?) exact-nonnegative-integer?)
                   (any/c)
                   any/c)]
 )

