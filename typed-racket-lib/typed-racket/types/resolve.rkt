#lang racket/base
(require "../utils/utils.rkt")

(require (rep type-rep rep-utils)
         (env type-name-env)
         (utils tc-utils)
         (types utils current-seen)
         racket/match
         (contract-req)
         racket/format)

(provide resolve-name resolve-app resolvable?
         resolve-app-check-error
         resolver-cache-remove!
         current-check-polymorphic-recursion
         register-app-for-checking!
         check-registered-apps!)
(provide/cond-contract
 [resolve-once (Type? . -> . (or/c Type? #f))]
 [resolve (Type? . -> . Type?)])

(define-struct poly (name vars) #:prefab)

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

(define (resolve-name t)
  (match t
    [(Name/simple: (app lookup-type-name t)) (if (Type? t) t #f)]
    [_ (int-err "resolve-name: not a name ~a" t)]))

(define already-resolving? (make-parameter #f))

;; list of (cons/c App? syntax?) of parsed Apps
;; during early phase of typechecking,
;; `check-registered-apps!` consumes these to verify
;; they are correct
(define apps-to-check (box '()))

;; registers an App for checking
;; used while parsing types initially,
;; once all definitions are loaded, we can verify
;; Apps are well formed (i.e. take the correct number of args, etc)
(define (register-app-for-checking! app stx)
  (set-box! apps-to-check
            (cons (cons app stx)
                  (unbox apps-to-check))))

;; checks apps registered with `register-app-for-checking!`
(define (check-registered-apps!)
  (for* ([p (in-list (unbox apps-to-check))]
         [(app stx) (in-pair p)])
    (match app
      [(App: (? Name? rator) rands)
       (resolve-app-check-error rator rands stx)]))
  (set-box! apps-to-check '()))

(define (resolve-app-check-error rator rands stx)
  (parameterize ([current-orig-stx stx])
    (match rator
      [(Poly-unsafe: n _)
       (unless (= n (length rands))
         (tc-error (~a "wrong number of arguments to polymorphic type"
                       "\n  type: " rator
                       "\n  expected: " n
                       "\n  given: " (length rands)
                       "\n  arguments...: " rands)))]
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
                       "\n  arguments...: " rands)))
       ;; Does not allow polymorphic recursion since both type
       ;; inference and equirecursive subtyping for polymorphic
       ;; recursion are difficult.
       ;;
       ;; Type inference is known to be undecidable in general, but
       ;; practical algorithms do exist[1] that do not diverge in
       ;; practice.
       ;;
       ;; It is possible that equirecursive subtyping with polymorphic
       ;; recursion is as difficult as equivalence of DPDAs[2], which is
       ;; known to be decidable[3], but good algorithms may not exist.
       ;;
       ;; [1] Fritz Henglein. "Type inference with polymorphic recursion"
       ;;     TOPLAS 1993
       ;; [2] Marvin Solomon. "Type definitions with parameters"
       ;;     POPL 1978
       ;; [3] Geraud Senizergues.
       ;;     "L(A)=L(B)? decidability results from complete formal systems"
       ;;     TCS 2001.
       ;;
       ;; check-argument : Type Id -> Void
       ;; Check argument to make sure there's no polymorphic recursion
       (define (check-argument given-type arg-name)
         (define ok?
           (or (F? given-type)
               (not (member (syntax-e arg-name) (fv given-type)))))
         (unless ok?
           (tc-error (~a "recursive type cannot be applied at a"
                         " different type in its recursive invocation"
                         "\n  type: " rator
                         "\n  new argument name: " arg-name
                         "\n  new argument: " given-type
                         "\n  new arguments...: " rands))))
       (match (current-check-polymorphic-recursion)
         [`#s(poly-rec-info ,same-component? ,current-vars)
          #:when (same-component? name-id)
          (for* ([rand (in-list rands)]
                 [var (in-list current-vars)])
            (check-argument rand var))]
         [_ (void)])]
      [(? Mu?) (void)]
      [(? App?) (void)]
      [(? Error?) (void)]
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
       (let ([r (resolve-name rator)])
         (and r (resolve-app r rands stx)))]
      [(? Poly?) (instantiate-poly rator rands)]
      [(? Mu?) (resolve-app (unfold rator) rands stx)]
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
