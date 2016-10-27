#lang racket/base

;; Provides functionality to take a static contract and turn it into a regular contract.

(require
  "../utils/utils.rkt"
  (utils fid-hset fid-hash)
  racket/match
  racket/contract
  racket/syntax
  (for-template racket/base racket/contract)
  "combinators.rkt"
  "combinators/name.rkt"
  "combinators/case-lambda.rkt"
  "combinators/parametric.rkt"
  "kinds.rkt"
  "parametric-check.rkt"
  "structures.rkt"
  "constraints.rkt"
  "equations.rkt")

(provide/cond-contract
 [instantiate
     (parametric->/c (a) ((static-contract? (-> #:reason (or/c #f string?) a))
                          (contract-kind? #:cache hash?)
                          . ->* . (or/c a (list/c (listof syntax?) syntax?))))]
 [should-inline-contract? (-> syntax? boolean?)])

;; Providing these so that tests can work directly with them.
(module* internals #f
  (provide compute-constraints
           instantiate/inner))

;; kind is the greatest kind of contract that is supported, if a greater kind would be produced the
;; fail procedure is called.
;;
;; The cache is used to share contract definitions across multiple calls to
;; type->contract in a given contract fixup pass. If it's #f then that means don't
;; do any sharing (useful for testing).
(define (instantiate sc fail [kind 'impersonator] #:cache [cache #f])
  (if (parametric-check sc)
      (fail #:reason "multiple parametric contracts are not supported")
      (with-handlers [(exn:fail:constraint-failure?
                        (lambda (exn) (fail #:reason (exn:fail:constraint-failure-reason exn))))]
        (instantiate/inner sc
          (compute-recursive-kinds
            (contract-restrict-recursive-values (compute-constraints sc kind)))
          cache))))

;; computes the definitions that are in / used by `sc`
;; `(get-all-name-defs)` is not what we want directly, since it also includes
;; definitions that were optimized away
;; we restrict it to only variables bound in `sc`
(define/cond-contract (compute-defs sc)
  (-> static-contract? (hash/c (listof identifier?)
                               (list/c static-contract?
                                       static-contract?
                                       static-contract?)))
  ;; all-name-defs : (listof (list/c (listof identifier?)
  ;;                                 static-contract?
  ;;                                 static-contract?
  ;;                                 static-contract?))
  (define all-name-defs (get-all-name-defs))
  ;; all-name-defs maps lists of ids to defs
  ;; we want to match if any id in the list matches
  (define (ref b)
    (for/first ([ids/contracts (in-list all-name-defs)]
                #:when (for/or ([k* (in-list (car ids/contracts))])
                         (free-identifier=? b k*)))
      ids/contracts))
  (define bound '())
  ;; ignores its second argument (variance, passed by sc-traverse)
  (let loop ([sc sc] [_ #f])
    (match sc
      [(name/sc: name*)
       (unless (member name* bound free-identifier=?)
         (set! bound (cons name* bound))
         ;; traverse what `name` refers to
         (define r (ref name*))
         ;; ref returns a rib, get the one definition we want
         (define target (for/first ([k (car r)]
                                    [v (cdr r)]
                                    #:when (free-identifier=? name* k))
                          v))
         (loop target #f))]
      [else (sc-traverse sc loop)]))
  (for*/hash ([b (in-list bound)]
              [v (in-value (ref b))]
              #:when v)
    (values (car v) (cdr v))))

(define (compute-constraints sc max-kind)
  (define memo-table (make-hash))
  (define name-defs (compute-defs sc))
  (define (recur sc)
    (cond [(hash-ref memo-table sc #f)]
          [else
           (define result
             (match sc
               [(recursive-sc names values body)
                (close-loop names (map recur values) (recur body))]
               [(? sc?)
                (sc->constraints sc recur)]))
           (hash-set! memo-table sc result)
           result]))
  (define constraints
    (if (null? name-defs)
        (recur sc)
        (close-loop (apply append (hash-keys name-defs))
                    (map recur (apply append (hash-values name-defs)))
                    (recur sc))))
  (validate-constraints (add-constraint constraints max-kind))
  constraints)


(define/cond-contract (compute-recursive-kinds recursives)
  (-> (fid-hashof any/c) (fid-hashof any/c))
  (define eqs (make-equation-set))
  (define vars
    (for*/fold ([h (fid-hash)])
               ([b (in-fid-hash-buckets recursives)]
                [name/val (in-fid-hash-bucket b)])
      (fid-hash-set h (car name/val) (add-variable! eqs 'flat))))

  (define (lookup id)
    (variable-ref (fid-hash-ref vars id)))

  (for* ([b (in-fid-hash-buckets recursives)]
         [name/val (in-fid-hash-bucket b)])
    (match (cdr name/val)
      [(kind-max others max)
       (add-equation! eqs
          (fid-hash-ref vars (car name/val))
          (λ ()
            (apply combine-kinds max (fid-hset-map others lookup))))]))
  (define var-values (resolve-equations eqs))
  (for*/fold ([t (fid-hash)])
             ([b (in-fid-hash-buckets vars)]
              [name/var (in-fid-hash-bucket b)])
    (fid-hash-set t (car name/var) (hash-ref var-values (cdr name/var)))))


(define (instantiate/inner sc recursive-kinds cache)
  (define bound-names (make-parameter null))
  ;; sc-queue : records the order in which to return syntax objects
  (define sc-queue null)
  ;; top-level? is #t only for the first call and not for recursive
  ;; calls, which helps for inlining
  (define (recur sc [top-level? #f])
    (cond [(and cache (hash-ref cache sc #f)) => car]
          [(arr/sc? sc) (make-contract sc)]
          [(or (parametric->/sc? sc) (sealing->/sc? sc))
           (match-define (or (parametric->/sc: vars _)
                             (sealing->/sc: vars _ _))
                         sc)
           (parameterize ([bound-names (append vars (bound-names))])
             (make-contract sc))]
          ;; If any names are bound, the contract can't be shared
          ;; becuase it depends on the scope it's in
          [(ormap (λ (n) (name-free-in? n sc)) (bound-names))
           (make-contract sc)]
          [else
           (define ctc (make-contract sc))
           (cond [(and ;; when a contract benefits from inlining
                       ;; (e.g., ->) and this contract appears
                       ;; directly in a define-module-boundary-contract
                       ;; position (i.e, top-level? is #t) then
                       ;; don't generate a new identifier for it
                       (or (not (should-inline-contract? ctc))
                           (not top-level?))
                       cache)
                  (define fresh-id (generate-temporary))
                  (hash-set! cache sc (cons fresh-id ctc))
                  (set! sc-queue (cons sc sc-queue))
                  fresh-id]
                 [else ctc])]))
  (define (make-contract sc)
    (match sc
      [(recursive-sc names values body)
       (define raw-names (generate-temporaries names))
       (define raw-bindings
         (parameterize ([bound-names (append names (bound-names))])
           (for/list ([raw-name (in-list raw-names)]
                      [value (in-list values)])
             #`[#,raw-name #,(recur value)])))
       (define bindings
         (for/list ([name (in-list names)]
                    [raw-name (in-list raw-names)])
            #`[#,name (recursive-contract #,raw-name
                                            #,(kind->keyword
                                                (fid-hash-ref recursive-kinds name)))]))
       #`(letrec (#,@bindings #,@raw-bindings)
           #,(parameterize ([bound-names (append names (bound-names))])
               (recur body)))]
      [(? sc? sc)
       (sc->contract sc recur)]))
  (define ctc (recur sc #t))
  (define name-defs (compute-defs sc))
  ;; These are extra contract definitions for the name static contracts
  ;; that are used for this type. Since these are shared across multiple
  ;; contracts from a single contract fixup pass, we use the name-defined
  ;; table to see if we've already defined it. If so, we avoid duplicating
  ;; the definition later.
  (define extra-defs
    (cond [(null? name-defs) null]
          [else
           (define names (for/fold ([l '()])
                                   ([names (in-hash-keys name-defs)])
                           (append names l)))
           (for/list ([name (in-list names)]
                      [sc   (in-list (for/fold ([l '()])
                                               ([names (in-hash-values name-defs)])
                                       (append names l)))]
                      #:unless (lookup-name-defined name))
             (set-name-defined name)
             #`(define #,name
                 (recursive-contract #,(recur sc)
                                     #,(kind->keyword (fid-hash-ref recursive-kinds name)))))]))
  (list (append ;; These contracts are sub-contract definitions used to
                ;; increase sharing among contracts in a given fixup pass
                extra-defs
                (for/list ([sc (in-list (reverse sc-queue))])
                  (match-define (cons id ctc) (hash-ref cache sc))
                  #`(define #,id #,ctc)))
        ctc))

;; Determine whether the given contract syntax should be inlined or not.
(define (should-inline-contract? stx)
  (or
   ;; no need to generate an extra def for things that are already identifiers
   (identifier? stx)
   ;; ->* are handled specially by the contract system
   (let ([sexp (syntax-e stx)])
     (and (pair? sexp)
          (or (free-identifier=? (car sexp) #'->)
              (free-identifier=? (car sexp) #'->*))))))

;; determine if a given name is free in the sc
(define (name-free-in? name sc)
  (let/ec escape
    (define/match (free? sc _)
      [((or (recursive-sc-use name*)
            (parametric-var/sc: name*)
            (sealing-var/sc: name*)
            (name/sc: name*))
        _)
       (when (free-identifier=? name name*)
         (escape #t))]
      [(_ _) (sc-traverse sc free?)])
    (free? sc 'dummy)
    #f))
