#lang racket/unit

(require "../utils/utils.rkt"
         (except-in (types utils abbrev prop-ops overlap type-table)
                    -> ->* one-of/c)
         (only-in (types abbrev) (-> t:->) [->* t:->*])
         (private type-annotation parse-type syntax-properties)
         (env lexical-env type-alias-helper mvar-env
              global-env scoped-tvar-env 
              signature-env signature-helper
              type-env-structs)
         (rep prop-rep object-rep type-rep)
         syntax/free-vars
         (typecheck signatures tc-metafunctions tc-subst internal-forms tc-envops)
         (utils tarjan)
         racket/match (contract-req)
         syntax/parse syntax/stx
         syntax/id-table
         ;; For internal type forms
         (for-template (only-in racket/base define-values)))

(require-for-cond-contract (rep type-rep))

(import tc-expr^)
(export tc-let^)

;; consolidate-bound-ids-info
;;
;; This function can be viewed as a helper to
;; T-Let from the formalism (it's actually a helper
;; for 'check-let-body' in this file).
;;
;; It takes as arguments:
;;
;; namess : the local names bound by a let-values
;; (note: all let's are special cases of let-values)
;;
;; rhs-typess : the types (acutally tc-results) of the
;; expressions whose values are assigned
;; to the local identifiers found in 'namess'
;;
;; returns : 4 lists of index-associated values:
;;           names
;;           types
;;           aliased-objs
;;           props
;;  - names is the idents bound for the body of this let
;;    (i.e. (flatten namess))
;;  - types[i] is the type which should be added to Γ for names[i].
;;  - aliased-objs[i] is the object which names[i] is an alias for,
;;    where Empty means it aliases no syntactic object.
;;  - props[i] is the logical information that holds for the
;;    body of the let which we learned while typechecking the
;;    rhs expression for names[i].
(define/cond-contract (consolidate-bound-ids-info namess rhs-typess)
  (->i ([nss (listof (listof identifier?))]
        [tss (listof (listof tc-result?))])
       #:pre (nss tss) (and (= (length nss) (length tss))
                            (for/and ([ns (in-list nss)]
                                      [ts (in-list tss)])
                              (= (length ns) (length ts))))
       (values [names (listof identifier?)]
               [types (listof Type?)]
               [aliased-objs (listof OptObject?)]
               [props (listof Prop?)]))
  (for*/lists (idents types aliased-objects propositions)
              ([(names results) (in-parallel (in-list namess) (in-list rhs-typess))]
               [(name result) (in-parallel (in-list names) (in-list results))])
    (match-define (tc-result: type p+/p- obj) result)
    (define mutated? (is-var-mutated? name))
    ;; n-obj is the object naming n (unless n is mutated, then
    ;; its -empty-obj)
    (define aliased-obj (if mutated? -empty-obj obj))
    (define-values (p+ p-) (match p+/p-
                             [(PropSet: p+ p-) (values p+ p-)]
                             ;; it's unclear if this 2nd clause is necessary any more.
                             [_ (values -tt -tt)]))
    (define-values (type* extracted-props)
      (cond
        [(Object? aliased-obj) (extract-props aliased-obj type)]
        [else (extract-props (-id-path name) type)]))
    (define truthy-prop (cond ;; if n can't be #f, we can assume p+
                          [(not (overlap? type* -False)) p+]
                          [mutated? -tt]
                          [else ;; otherwise we know ((o ∉ #f) ∧ p+) ∨ ((o ∈ #f) ∧ p-)
                           (let ([obj (if (Object? obj) obj name)])
                             (-or (-and (-not-type obj -False) p+)
                                  (-and (-is-type obj -False) p-)))]))
    (define prop (apply -and truthy-prop extracted-props))
    (values name type aliased-obj prop)))

;; check-let-body
;; 
;; Checks that the body has the expected type with the info
;; from the newly bound ids appropriately added to the type environment.
;;
;; bound-idss : all of the local names bound by a let
;;
;; bound-resultss : the types (tc-results actually) of all of the names
;; in 'bound-idss'. The info in 'bound-resultss' is what is used to
;; extend Γ while typechecking the environment. See the helper
;; function 'consolidate-bound-ids-info'.
(define/cond-contract (check-let-body bound-idss bound-resultss body expected 
                                      #:before-check-body [pre-body-thunk void])
  (((listof (listof identifier?))
    (listof (listof tc-result?))
    syntax?
    (or/c #f tc-results/c))
   (#:before-check-body (-> any/c))
   . ->* .
   tc-results/c)
  (define-values (idents types aliased-objs props)
    (consolidate-bound-ids-info bound-idss bound-resultss))
  (define ids-to-erase
    (for/list ([id (in-list idents)]
               [obj (in-list aliased-objs)]
               #:when (Empty? obj))
      id))
  ;; extend the lexical environment for checking the body
  ;; with types and potential aliases
  (with-extended-lexical-env
    [#:identifiers idents
     #:types types
     #:aliased-objects aliased-objs]
    (erase-identifiers
     (with-lexical-env+props
       props
       #:expected expected
       ;; if a let rhs does not return, the body isn't checked
       #:unreachable (for ([form (in-list (syntax->list body))])
                       (register-ignored! form))
       ;; Perform additional context-dependent checking that needs to be done
       ;; before checking the body
       (pre-body-thunk)
       ;; typecheck the body
       (tc-body/check body expected))
     ids-to-erase)))

(define (tc-expr/maybe-expected/t e names)
  (syntax-parse names
    [(i:typed-id^ ...)
     (tc-expr/check e (-values (attribute i.type)))]
    [_ (tc-expr e)]))


(define (register-aliases-and-declarations names exprs)
  ;; Collect the declarations, which are represented as expressions.
  ;; We put them back into definitions to reuse the existing machinery
  (define-values (type-aliases declarations signature-forms)
    (for/fold ([aliases '()] [declarations '()] [signature-forms '()])
              ([body (in-list exprs)])
      (syntax-parse #`(define-values () #,body)
        [t:type-alias
         (values (cons #'t aliases) declarations signature-forms)]
        [t:type-declaration
         (values aliases (cons (list #'t.id #'t.type) declarations) signature-forms)]
        [t:typed-define-signature
           (values aliases declarations (cons #'t signature-forms))]
        [_ (values aliases declarations signature-forms)])))

  ;; add signature names to the signature environment, deferring type parsing
  ;; until after aliases are registered to allow mutually recursive references
  ;; between signatures and type aliases
  (for/list ([sig-form (in-list (reverse signature-forms))])
    (parse-and-register-signature! sig-form))

  (define-values (alias-names alias-map) (get-type-alias-info type-aliases))
  (register-all-type-aliases alias-names alias-map)

  (for ([declaration declarations])
    (match-define (list id type) declaration)
    (register-type-if-undefined id (parse-type type))
    (register-scoped-tvars id (parse-literal-alls type)))

  ;; add scoped type variables, before we get to typechecking
  ;; FIXME: can this pass be fused with the one immediately above?
  (for ([n (in-list names)] [b (in-list exprs)])
    (syntax-case n ()
      [(var) (add-scoped-tvars b (lookup-scoped-tvars #'var))]
      [_ (void)]))

  ;; Finalize signatures, by parsing member types
  (finalize-signatures!))

;; The `thunk` argument is run only for its side effects 
;; It is used to perform additional context-dependent checking
;; within the context of a letrec body.
;; For example, it is used to typecheck units and ensure that exported
;; variables are exported at the correct types
(define (tc/letrec-values namess exprs body [expected #f] [check-thunk void])
  (let* ([namess (stx-map syntax->list namess)]
         [orig-flat-names (apply append namess)]
         [exprs (syntax->list exprs)])
    (register-aliases-and-declarations namess exprs)
    
    ;; First look at the clauses that do not bind the letrec names
    (define all-clauses
      (for/list ([name-lst (in-list namess)]
                 [expr (in-list exprs)])
        (lr-clause name-lst expr)))

    (define-values (ordered-clauses remaining)
      (get-non-recursive-clauses all-clauses orig-flat-names))

    (define-values (remaining-names remaining-exprs)
      (for/lists (_1 _2) ([remaining-clause (in-list remaining)])
        (match-define (lr-clause name expr) remaining-clause)
        (values name expr)))

    ;; Check those and then check the rest in the extended environment
    (check-non-recursive-clauses
     ordered-clauses
     (λ ()
       ;; types the user gave.
       (define given-rhs-types (map (λ (l) (map -tc-result (map get-type l))) remaining-names))
       (check-let-body
        remaining-names
        given-rhs-types
        body
        expected
        #:before-check-body
        (λ () (begin (for ([expr (in-list remaining-exprs)]
                           [results (in-list given-rhs-types)])
                       (match results
                         [(list (tc-result: ts fs os) ...)
                          (tc-expr/check expr (ret ts fs os))]))
                     (check-thunk))))))))

;; An lr-clause is a
;;   (lr-clause (Listof Identifier) Syntax)
;;
;; interp. represents a letrec binding
(struct lr-clause (names expr) #:transparent)

;; get-non-recursive-clauses : (Listof lr-clause) (Listof Identifier) ->
;;                             (Listof lr-clause) (Listof lr-clause)
;; Find letrec-values clauses that do not create variable cycles. Return
;; both the non-recursive clauses and the remaining recursive ones.
(define (get-non-recursive-clauses clauses flat-names)

  ;; First, filter out clauses with no names. Don't do cycle checking on
  ;; these because they trivially don't form any.
  (define-values (*non-binding *other-clauses)
    (for/fold ([non-binding '()] [other-clauses '()])
              ([clause clauses])
      (match-define (lr-clause names _) clause)
      (if (null? names)
          (values (cons clause non-binding) other-clauses)
          (values non-binding (cons clause other-clauses)))))
  (define-values (non-binding other-clauses)
    (values (reverse *non-binding) (reverse *other-clauses)))

  ;; Set up vertices for Tarjan's algorithm, where each letrec-values
  ;; clause is a vertex but mapped in the table for each of the clause names
  (define vertices (make-free-id-table))
  (for ([clause other-clauses])
    (match-define (lr-clause names expr) clause)
    (define relevant-free-vars
      (for/list ([var (in-list (free-vars expr))]
                 #:when (member var flat-names free-identifier=?))
        var))
    (define vertex (make-vertex clause relevant-free-vars))
    (for ([name (in-list names)])
      (free-id-table-set! vertices name vertex)))

  (define components (tarjan vertices))

  ;; no-self-cycle? : (Vertex Id (Listof Id)) -> Boolean
  (define (no-self-cycle? vertex)
    (match-define (lr-clause names _) (vertex-data vertex))
    (for/and ([id (in-list names)])
      (andmap (λ (id2) (not (free-identifier=? id id2)))
              (vertex-adjacent vertex))))

  ;; The components with only one entry are non-recursive if they also
  ;; contain no self-cycles.
  (define-values (non-recursive remaining)
    (for/fold ([non-recursive '()]
               [remaining '()])
              ([component components])
      (cond [(and (= (length component) 1)
                  (no-self-cycle? (car component)))
             (values (cons (vertex-data (car component)) non-recursive)
                     remaining)]
            [else
             (values non-recursive
                     (append (map vertex-data component)
                             remaining))])))
  (values (append non-recursive non-binding)
          remaining))

;; check-non-recursive-clauses : (Listof lr-clause) (-> tc-results) -> tc-results
;; Given a list of non-recursive clauses, check the clauses in order then call k
;; in the built up environment.
(define (check-non-recursive-clauses clauses k)
  (let loop ([clauses clauses])
    (cond [(null? clauses) (k)]
          [else
           (match-define (lr-clause names expr) (car clauses))
           (match-define (list (tc-result: ts fs os) ...)
             (get-type/infer names expr
                             (lambda (e) (tc-expr/maybe-expected/t e names))
                             tc-expr/check))
           (with-extended-lexical-env
             [#:identifiers names
              #:types ts]
             (substitute-names (loop (cdr clauses))
                               names
                               os))])))

;; this is so match can provide us with a syntax property to
;; say that this binding is only called in tail position
(define ((tc-expr-t/maybe-expected expected) e)
  (syntax-parse e #:literal-sets (kernel-literals)
    [(~and (#%plain-lambda (fmls:type-annotation^ ...) _) _:tail-position^)
     #:when expected
     (define arg-tys (attribute fmls.type))
     (tc-expr/check e (ret (t:->* arg-tys (tc-results->values expected))))]
    [_:tail-position^
     #:when expected
     (tc-expr/check e expected)]
    [_ (tc-expr e)]))


(define (tc/let-values namess exprs body [expected #f])
  (let* (;; a list of each name clause
         [namess (stx-map syntax->list namess)]
         ;; all the trailing expressions - the ones actually bound to the names
         [exprs (syntax->list exprs)])

    (register-aliases-and-declarations namess exprs)

    ;; the annotated types of the name (possibly using the inferred types)
    (let ([resultss (for/list ([names (in-list namess)] [e (in-list exprs)])
                      (get-type/infer names e (tc-expr-t/maybe-expected expected)
                                      tc-expr/check))])
      (check-let-body namess resultss body expected))))
