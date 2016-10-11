#lang racket/unit

(require "../utils/utils.rkt"
         (except-in (types utils abbrev prop-ops overlap type-table)
                    -> ->* one-of/c)
         (only-in (types abbrev) (-> t:->) [->* t:->*])
         (private type-annotation parse-type syntax-properties)
         (env lexical-env type-alias-helper mvar-env
              global-env scoped-tvar-env 
              signature-env signature-helper)
         (rep prop-rep object-rep type-rep)
         syntax/free-vars
         (typecheck signatures tc-metafunctions tc-subst internal-forms tc-envops)
         (utils tarjan)
         racket/match (contract-req)
         racket/list
         syntax/parse syntax/stx
         syntax/id-table
         ;; For internal type forms
         (for-template (only-in racket/base define-values)))

(require-for-cond-contract (rep type-rep))

(import tc-expr^)
(export tc-let^)

;; get-names+objects (listof (listof identifier?)) (listof tc-results?) -> (listof (list/c identifier Object?))
;; Given a list of bindings and the tc-results for the corresponding expressions, return a list of
;; tuples of the binding-name and corresponding objects from the results.
;; This is used to replace the names with the objects after the names go out of scope.
(define (get-names+objects namess results)
  (append*
    (for/list ([names namess] [results results])
      (match results
        [(list (tc-result: _ _ os) ...)
         (map list names os)]))))

;; Checks that the body has the expected type when names are bound to the types spcified by results.
;; The exprs are also typechecked by using expr->type.
;; TODO: make this function sane.
;; The `check-thunk` argument serves the same purpose as in tc/letrec-values
(define/cond-contract (do-check expr->type namess expected-results exprs body expected 
                                [check-thunk void])
  (((syntax? tc-results/c . -> . any/c)
    (listof (listof identifier?)) (listof (listof tc-result?))
    (listof syntax?) syntax? (or/c #f tc-results/c))
   ((-> any/c))
   . ->* .
   tc-results/c)
  (with-cond-contract t/p ([expected-types (listof (listof Type?))]
                           [objs           (listof (listof OptObject?))]
                           [props          (listof (listof Prop?))])
    (define-values (expected-types objs props)
      (for/lists (e o p)
        ([e-r   (in-list expected-results)]
         [names (in-list namess)])
        (match e-r
          [(list (tc-result: e-ts (PropSet: fs+ fs-) os) ...)
           (values e-ts
                   (map (λ (o n t) (if (or (is-var-mutated? n) (F? t)) -empty-obj o)) os names e-ts)
                   (apply append
                          (for/list ([n (in-list names)]
                                     [t (in-list e-ts)]
                                     [f+ (in-list fs+)]
                                     [f- (in-list fs-)]
                                     [o (in-list os)])
                            (cond
                              [(not (overlap? t (-val #f)))
                               (list f+)]
                              [(is-var-mutated? n)
                               (list)]
                              ;; n is being bound to an expression w/ object o, no new info 
                              ;; is required due to aliasing (note: we currently do not
                              ;; alias objects typed as type variables)
                              [(and (Path? o) (not (F? t))) (list)]
                              ;; n is being bound to an expression w/o an object (or whose
                              ;; type is a type variable) so create props about n
                              [else (list (-or (-and (-not-type n (-val #f)) f+)
                                               (-and (-is-type n (-val #f)) f-)))]))))]
          ;; amk: does this case ever occur?
          [(list (tc-result: e-ts #f _) ...)
           (values e-ts (make-list (length e-ts) -empty-obj) null)]))))
  ;; extend the lexical environment for checking the body
  ;; with types and potential aliases
  (let ([names (append* namess)]
        [objs (append* objs)])
    (with-lexical-env/extend-types+aliases
      names
      (append* expected-types)
      objs
      (replace-names
       names
       objs
       (with-lexical-env/extend-props
         (apply append props)
         ;; if a let rhs does not return, the body isn't checked
         #:unreachable (for ([form (in-list (syntax->list body))])
                         (register-ignored! form))
         ;; type-check the rhs exprs
         (for ([expr (in-list exprs)] [results (in-list expected-results)])
           (match results
             [(list (tc-result: ts fs os) ...)
              (expr->type expr (ret ts fs os))]))
         ;; Perform additional context-dependent checking that needs to be done
         ;; in the context of the letrec body
         (check-thunk)
         ;; typecheck the body
         (tc-body/check body (and expected (erase-props expected))))))))

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
  (let* ([names (stx-map syntax->list namess)]
         [orig-flat-names (apply append names)]
         [exprs (syntax->list exprs)])
    (register-aliases-and-declarations names exprs)
    
    ;; First look at the clauses that do not bind the letrec names
    (define all-clauses
      (for/list ([name-lst names] [expr exprs])
        (lr-clause name-lst expr)))

    (define-values (ordered-clauses remaining)
      (get-non-recursive-clauses all-clauses orig-flat-names))

    (define-values (remaining-names remaining-exprs)
      (for/lists (_1 _2) ([remaining-clause remaining])
        (match-define (lr-clause name expr) remaining-clause)
        (values name expr)))

    ;; Check those and then check the rest in the extended environment
    (check-non-recursive-clauses
     ordered-clauses
     (lambda ()
       (cond
         ;; after everything, check the body expressions
         [(null? remaining-names)
          (check-thunk)
          (tc-body/check body (and expected (erase-props expected)))]
         [else
          (define flat-names (apply append remaining-names))
          (do-check tc-expr/check
                    remaining-names
                    ;; types the user gave.
                    (map (λ (l) (map tc-result (map get-type l))) remaining-names)
                    remaining-exprs body expected
                    check-thunk)])))))

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
           (with-lexical-env/extend-types 
            names 
            ts
            (replace-names names
                           os
                           (loop (cdr clauses))))])))

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
         [names (stx-map syntax->list namess)]
         ;; all the trailing expressions - the ones actually bound to the names
         [exprs (syntax->list exprs)])

    (register-aliases-and-declarations names exprs)

    (let* (;; the types of the exprs
           #;[inferred-types (map (tc-expr-t/maybe-expected expected) exprs)]
           ;; the annotated types of the name (possibly using the inferred types)
           [results (for/list ([name (in-list names)] [e (in-list exprs)])
                      (get-type/infer name e (tc-expr-t/maybe-expected expected)
                                      tc-expr/check))])
      (do-check void names results exprs body expected))))
