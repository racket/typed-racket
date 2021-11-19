#lang racket/base

;; This module provides helper functions for type aliases

(require "../utils/utils.rkt"
         "../utils/tarjan.rkt"
         "../utils/tc-utils.rkt"
         "type-alias-env.rkt"
         "type-name-env.rkt"
         "../rep/type-rep.rkt"
         "../rep/type-constr.rkt"
         "tvar-env.rkt"
         "type-constr-env.rkt"
         "../private/parse-type.rkt"
         "../typecheck/internal-forms.rkt"
         "../types/resolve.rkt"
         "../types/base-abbrev.rkt"
         "../types/substitute.rkt"
         racket/list
         racket/match
         racket/set
         racket/dict
         racket/function
         syntax/id-table
         syntax/parse
         (for-template
          "../typecheck/internal-forms.rkt"
          racket/base))

(provide find-strongly-connected-type-aliases
         register-all-type-aliases
         parse-type-alias)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Data definitions for aliases
;;
;; A TypeAliasInfo is a (list Syntax (Listof Identifier))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dict<Id, (List Type Listof<Id>)> -> Listof<Listof<Id>>
;; Find strongly connected type aliases in order to
;; find mutually recursive aliases
;;
;; Returns the components in topologically sorted order
(define (find-strongly-connected-type-aliases dep-map)
  (define vertex-map (make-free-id-table))
  (for ([(id adjacent) (in-dict dep-map)])
    (free-id-table-set! vertex-map id (make-vertex id adjacent)))
  (define components (tarjan vertex-map))
  ;; extract the identifiers out of the results since we
  ;; don't need the whole vertex
  (for/list ([component (in-list components)])
    (map vertex-data component)))


;; register-all-type-aliases : Listof<Syntax> -> Void
;;
;; register all type alias definitions carried by the input syntaxes
(define (register-all-type-aliases type-aliases)
  (parameterize ([incomplete-name-alias-map (make-free-id-table)])
    (define-values (type-alias-names type-alias-map)
      (for/lists (_1 _2 #:result (values _1 (make-free-id-table
                                             (map (lambda (a)
                                                    (cons (car a) a))
                                                  _2))))
                 ([type-alias (in-list type-aliases)])
        (define-values (id type-stx args) (parse-type-alias type-alias))
        ;; start registering type alias names
        (start-type-alias-registration! id (make-Name id (length args) #f))
        (values id (list id type-stx args))))
    (register-all-type-alias-info type-alias-names type-alias-map)
    (unless (zero? (free-id-table-count (incomplete-name-alias-map)))
      (define names (free-id-table-keys (incomplete-name-alias-map)))
      (int-err "not all type alias names are fully registered: ~n ~a"
               names))))

;; Identifier -> Type
;; Construct a fresh placeholder type
(define (make-placeholder-type id)
  (make-Opaque id))

;; register-all-type-aliases : Listof<Id> Dict<Id, TypeAliasInfo> -> Void
;;
;; Given parsed type aliases and a type alias map, do the work
;; of actually registering the type aliases. If struct names or
;; other definitions need to be registered, do that before calling
;; this function.
(define (register-all-type-alias-info type-alias-names type-alias-map)
  ;; Find type alias dependencies
  ;; The two maps defined here contains the dependency structure
  ;; of type aliases in two senses:
  ;;   (1) other type aliases referenced in a type alias
  ;;   (2) other type aliases referenced by some class in a
  ;;       type alias in a #:implements clause
  ;;
  ;; The second is necessary in order to prevent recursive
  ;; #:implements clauses and to determine the order in which
  ;; recursive type aliases should be initialized.
  (define-values (type-alias-dependency-map type-alias-class-map type-alias-productivity-map)
    (for/lists (_1 _2 _3 #:result (values (make-free-id-table _1)
                                          (make-free-id-table _2)
                                          (make-free-id-table _3)))
               ([(name alias-info) (in-free-id-table type-alias-map)])
      (match-define (list _ type-stx args) alias-info)
      (define-values (links classes productivity)
        (parse-for-effects name (cons args type-stx)))

      (define pre-dependencies
        (remove-duplicates links free-identifier=?))
      (define (filter-by-type-alias-names names)
        (for/list ([id (in-list names)]
                   #:when (memf (λ (id2) (free-identifier=? id id2))
                                type-alias-names))
          id))
      (define alias-dependencies
        (filter-by-type-alias-names pre-dependencies))
      (define class-dependencies
        (filter-by-type-alias-names classes))
      (values (cons name alias-dependencies)
              (cons name class-dependencies)
              (cons name productivity))))

  (define components
    (find-strongly-connected-type-aliases type-alias-dependency-map))

  (define class-components
    (find-strongly-connected-type-aliases type-alias-class-map))

  ;; helper function for defining singletons
  (define (has-self-cycle? component [map type-alias-dependency-map])
    (define id (car component))
    (memf (λ (id2) (free-identifier=? id id2))
          (free-id-table-ref map id)))

  ;; A singleton component can be either a self-cycle or a node that
  ;; that does not participate in cycles, so we disambiguate
  (define-values (acyclic-singletons recursive-aliases)
    (for/fold ([singletons '()] [other '()])
              ([component (in-list components)])
      (if (and (= (length component) 1)
               (not (has-self-cycle? component)))
          (values (cons (car component) singletons) other)
          (values singletons (append component other)))))

  ;; Check that no #:implements clauses are recursive
  (define counterexample
    (for/or ([component (in-list class-components)])
      (and (or (not (= (length component) 1))
               (has-self-cycle? component type-alias-class-map))
           component)))
  (when counterexample
    (tc-error/stx
     (car counterexample)
     "Recursive #:implements clause not allowed"))

  ;; Split recursive aliases into those involving classes
  ;; (in reverse topological order) and the rest of the aliases
  (define class-aliases
    (for/list ([component (in-list (reverse class-components))]
               #:when (member (car component)
                              recursive-aliases
                              free-identifier=?))
      (car component)))
  (define other-recursive-aliases
    (for/list ([alias (in-list recursive-aliases)]
               #:unless (member alias
                                class-aliases
                                free-identifier=?))
      alias))


  ;; Actually register recursive type aliases
  (define name-types
    (for/list ([id (in-list recursive-aliases)])
      (define record (free-id-table-ref type-alias-map id))
      (match-define (list _ _ args) record)
      (define name-type (make-Name id (length args) #f))
      (register-resolved-type-alias id name-type)
      ;; The `(make-placeholder-type id)` expression is used to make sure
      ;; that unions don't collapse the aliases too soon. This is a dummy
      ;; value that's used until the real type is found in the pass below.
      ;;
      ;; A type name should not be registered for non-recursive aliases
      ;; because dummy values will leak due to environment serialization.
      (register-type-name
       id
       (if (null? args)
           (make-placeholder-type id)
           ;; TODO: we should simply gather the names and put them into kind-related
           ;; enviroment
           (make-Poly (map syntax-e args) (make-placeholder-type id))))
      name-type))

  ;; Register non-recursive type aliases
  ;;
  ;; Note that the connected component algorithm returns results
  ;; in topologically sorted order, so we want to go through in the
  ;; reverse order of that to avoid unbound type aliases.
  (for ([id (in-list acyclic-singletons)])
    (match-define (list _ type-stx args) (free-id-table-ref type-alias-map id))
    (cond
      [(not (null? args))
       (define ty-op (parse-type-operator-abstraction id args type-stx #f
                                                      type-alias-productivity-map))
       (register-type-constructor! id ty-op)]
      [else
       ;; id can be a simple abbreviation for another type constructor
       (define rv (parse-type-or-type-constructor type-stx))
       ((if (TypeConstructor? rv)
            register-type-constructor!
            register-resolved-type-alias) id rv)])
    (complete-type-alias-registration! id))

  ;; Clear the resolver cache of Name types from this block

  (define (reset-resolver-cache!) (resolver-cache-remove! name-types))
  (reset-resolver-cache!)

  ;; Checks whether two aliases are in the same connected component.
  ;; Used for the polymorphic recursion check below.
  (define (in-same-component? id id2)
    (for/or ([component (in-list (append components class-components))])
      (and (member id component free-identifier=?)
           (member id2 component free-identifier=?)
           #t)))

  ;; Finish registering recursive aliases
  ;; names-to-refine : Listof<Id>
  ;; types-to-refine : Listof<Type>
  ;; tvarss          : Listof<Listof<Symbol>>
  (define-values (type-records type-op-records)
    (for/fold ([type-records null]
               [type-op-records null]
               #:result
               (values (reverse type-records)
                       (reverse type-op-records)))
              ([id (in-list (append other-recursive-aliases class-aliases))])
      (define record (free-id-table-ref type-alias-map id))
      (match-define (list _ type-stx args) record)
      (if (null? args)
          (values (cons record type-records)
                  type-op-records)
          (values type-records
                  (cons record type-op-records)))))


  (define-values (names-to-refine types-to-refine tvarss)
    (for/lists (_1 _2 _3)
               ([record (in-list type-records)])
      (match-define (list id type-stx args) record)
      ;; TODO try parse-type
      (define type (parse-type type-stx type-alias-productivity-map))
      (reset-resolver-cache!)
      (register-type-name id type)
      (complete-type-alias-registration! id)
      (add-constant-variance! id args)
      (values id type (map syntax-e args))))

  ;; do a pass to refine the variance
  (refine-variance! names-to-refine types-to-refine tvarss)

  (define-values (productive unproductive)
    (partition (match-lambda
                 [(cons a _)
                  (equal? (free-id-table-ref type-alias-productivity-map a #f) #t)])
               type-op-records))

  (let (;; sort unproductive constructors by the number of dependent
        ;; user-defined constructors in increasing order
        [unproductive (sort unproductive <
                            #:key
                             (match-lambda
                               [(cons a _)
                                (length (free-id-table-ref type-alias-dependency-map a #f))]))])
    (for/list ([record (in-list (append productive unproductive))])
      (match-define (list id type-stx args) record)
      (define ty-op (parse-type-operator-abstraction id args type-stx
                                                     (lambda (x)
                                                       (define res (in-same-component? id x))
                                                       res)
                                                     type-alias-productivity-map))
      (register-type-constructor! id ty-op)
      (complete-type-alias-registration! id)
      (reset-resolver-cache!)
      (add-constant-variance! id args))))

;; Syntax -> Syntax Syntax (Listof Syntax)
;; Parse a type alias internal declaration
(define (parse-type-alias form)
  (syntax-parse form
    #:literal-sets (kernel-literals)
    #:literals (values)
    [t:type-alias
     (values #'t.name #'t.body (syntax-e #'t.params))]
    ;; this version is for `let`-like bodies
    [(begin (quote-syntax (define-type-alias-internal nm body args))
            (#%plain-app values))
     (values #'nm #'body (syntax-e #'args))]
    [_ (int-err "not define-type-alias")]))
