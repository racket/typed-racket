#lang racket/base
(require "../utils/utils.rkt")

(require "../rep/type-rep.rkt"
         "../rep/rep-utils.rkt"
         "../rep/type-constr.rkt"
         "../env/type-name-env.rkt"
         "../env/type-constr-env.rkt"
         (only-in "../env/type-alias-env.rkt" complete-name?)
         "../utils/tc-utils.rkt"
         "utils.rkt"
         "current-seen.rkt"
         racket/match
         (contract-req)
         racket/format)

(provide resolve-name resolve-app resolvable?
         resolve-app-check-error
         resolver-cache-remove!
         current-check-polymorphic-recursion)
(provide/cond-contract
 [resolve-once (Type? . -> . (or/c Type? #f))]
 [resolve (Type? . -> . Type?)])

(define-struct poly (name vars) #:prefab)

(define (resolvable? x)
  (or (Mu? x)
      (complete-name? x)
      (App? x)))

;; (Parameter (Option Poly-Rec-Info))
;; This parameter controls whether or not the resolving process
;; should check for polymorphic recursion in implicit recursive
;; type names. This should only need to be enabled at type alias
;; definition time.
;;
;; If not #f, it should be a record of a procedure that checks if an
;; alias is in the same connected component as the original alias
;; and a list of symbols that correspond to the type parameters of
;; the type being parsed.
(define current-check-polymorphic-recursion (make-parameter #f))

(define (resolve-name t [for-app #f])
  (match* (for-app t)
    [(#t (Name/simple: (app lookup-type-constructor (? TypeConstructor? t)))) t]
    [(_ (Name/simple: (app lookup-type-name t))) (if (Type? t) t #f)]
    [(_ _) (int-err "resolve-name: not a name ~a" t)]))

(define already-resolving? (make-parameter #f))

(define (resolve-app-check-error rator rands stx)
  (parameterize ([current-orig-stx stx])
    (match rator
      [(Name/struct: n) #:when (and (current-poly-struct)
                                    (free-identifier=? n (poly-name (current-poly-struct))))
       (define poly-num (length (poly-vars (current-poly-struct))))
       (if (= poly-num (length rands))
           (when (not (or (ormap Error? rands)
                          (andmap equal? rands
                                  (poly-vars (current-poly-struct)))))
             (tc-error (~a "structure type constructor applied to non-regular arguments"
                           "\n  type: " rator
                           "\n  arguments...: " rands)))
           (tc-error (~a "wrong number of arguments to structure type constructor"
                         "\n  type: " rator
                         "\n  expected: " poly-num
                         "\n  given: " (length rands)
                         "\n  arguments...: " rands)))]
      [(Name: name-id num-args _) #:when (> num-args 0)
       (define num-rands (length rands))
       (unless (= num-rands num-args)
         (tc-error (~a "wrong number of arguments to polymorphic type"
                       "\n  type: " rator
                       "\n  expected: " num-args
                       "\n  given: " num-rands
                       "\n  arguments...: " rands)))]
      [_ (tc-error/delayed (~a "type cannot be applied"
                               "\n  type: " rator
                               "\n  arguments...: " rands))])))



(define (resolve-app rator rands [stx #f])
  (define orig-stx (or stx (current-orig-stx)))
  (parameterize ([current-orig-stx orig-stx]
                 [already-resolving? #t])
    (resolve-app-check-error rator rands orig-stx)
    (match rator
      [(? Name?)
       (let ([r (resolve-name rator #t)])
         (and r
              (if (TypeConstructor? r)
                  (apply r rands)
                  (resolve-app r rands stx))))]
      [(App: r r*) (resolve-app (resolve-app r r* (current-orig-stx))
                                rands
                                (current-orig-stx))]
      [_ (tc-error (~a "cannot apply a non-polymorphic type"
                       "\n  type: " rator
                       "\n  arguments: " rands))])))


(define resolver-cache (make-hash))

;; unfolds a Mu, App, or Name, but returns #f if it
;; is a Name which has not yet been defined
(define (resolve-once t)
  (define r (hash-ref resolver-cache t #f))
  (or r
      (let ([r* (match t
                  [(Mu: _ _) (unfold t)]
                  [(App: r r*) (resolve-app r r* #f)]
                  [(? Name?) (resolve-name t)])])
        (when (and r* (not (currently-subtyping?)))
          (hash-set! resolver-cache t r*))
        r*)))

;; resolver-cache-remove! : (Listof Type) -> Void
;; Removes the given types from the resolver cache. This is
;; only used by recursive type alias set-up, which sometimes needs to
;; undo certain resolutions.
(define (resolver-cache-remove! keys)
  (for ([key (in-list keys)])
    (hash-remove! resolver-cache key)))

;; Repeatedly unfolds Mu, App, and Name constructors until the top type
;; constructor is not one of them.
;; Type? -> Type?
(define (resolve t)
  (cond
    [(resolvable? t)
     (define t* (resolve-once t))
     (if t* (resolve t*) t)]
    [else t]))
