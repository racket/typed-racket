#lang racket/unit

(require "../utils/utils.rkt"
         racket/list syntax/parse syntax/stx
         racket/match syntax/private/id-table
         racket/sequence
         (contract-req)
         (rep type-rep object-rep rep-utils)
         (rename-in (types abbrev utils)
                    [-> t:->]
                    [->* t:->*]
                    [one-of/c t:one-of/c])
         (private type-annotation syntax-properties)
         (types resolve type-table)
         (typecheck signatures tc-metafunctions tc-subst)
         (env lexical-env tvar-env index-env scoped-tvar-env)
         (utils tc-utils)
         (for-template racket/base)
         (for-syntax racket/base))

(import tc-expr^)
(export tc-lambda^)


(define-syntax-class cl-rhs
  #:literal-sets (kernel-literals)
  #:attributes (i cond)
  [pattern i:id #:attr cond #f]
  [pattern (if cond:expr e:expr i:id)])

(define-syntax-class rebuild-let*
  #:literal-sets (kernel-literals)
  #:attributes (mapping)
  (pattern (#%expression :rebuild-let*))
  (pattern (let-values ([(new-id) e:cl-rhs]) body:rebuild-let*)
           #:attr mapping (free-id-table-set (attribute body.mapping) #'e.i #'new-id))
  (pattern body:expr
           #:attr mapping (make-immutable-free-id-table)))

;; positional: (listof identifier?)
;; rest: id or #f
;; syntax: syntax? - the improper syntax list of identifiers
;; (i.e. (append positional (or id '())) but syntax)
(struct formals (positional rest syntax) #:transparent)

(define (make-formals stx)
  (let loop ([s stx] [acc null])
    (cond
      [(pair? s) (loop (cdr s) (cons (car s) acc))]
      [(null? s) (formals (reverse acc) #f stx)]
      [(pair? (syntax-e s)) (loop (stx-cdr s) (cons (stx-car s) acc))]
      [(null? (syntax-e s)) (formals (reverse acc) #f stx)]
      [else (formals (reverse acc) s stx)])))

;; Currently no support for objects representing the rest argument
(define (formals->objects f)
  (for/list ([i (in-list (formals-positional f))])
    (make-Path null i)))

(define (expected-str tys-len rst arg-len rest-id)
  (format "Expected function with ~a argument~a~a, but got function with ~a argument~a~a"
          tys-len
          (if (= tys-len 1) "" "s")
          (if rst
              " and a rest arg"
              "")
          arg-len
          (if (= arg-len 1) "" "s")
          (if rest-id " and a rest arg" "")))

;; tc-lambda-body: Typechecks the body with the given args and names
;;                 and returns the resulting Arrow?.
;; arg-names: The identifiers of the positional args
;; arg-types: The types of the positional args
;; rest-arg+type: Either #f for no rest argument or (cons rest-id rest-type)
;;                where rest-id is the identifier of the rest arg,
;;                and (ListOf rest-type) is the type that identifier would
;;                have in the function body
;; expected: The expected type of the body forms.
;; body: The body of the lambda to typecheck.
(define/cond-contract
  (tc-lambda-body arg-names arg-types
                  #:rest-id+type+body-type [rest-id+type+body-type #f]
                  #:expected [expected #f] body)
  (->* ((listof identifier?) (listof Type?) syntax?)
       (#:rest-id+type+body-type (or/c #f (list/c identifier? (or/c Rest? RestDots?) Type?))
        #:expected (or/c #f tc-results/c))
       Arrow?)

  (define-values (rst-id rst-type names types)
    (match rest-id+type+body-type
      [(list id rst body-type)
       (values id rst (cons id arg-names) (cons body-type arg-types))]
      [_ (values #f #f arg-names arg-types)]))

  (-Arrow
   arg-types
   (abstract-results
    (with-extended-lexical-env
        [#:identifiers names
         #:types types]
      (tc-body/check body expected))
    arg-names #:rest-id rst-id)
   #:rest rst-type))

;; check-clause: Checks that a lambda clause has arguments and body matching the expected type
;; arg-list: The identifiers of the positional args in the lambda form
;; rest-id: The identifier of the rest arg, or #f for no rest arg
;; body: The body of the lambda to typecheck.
;; arg-tys: The expected positional argument types.
;; rst: #f, expected rest arg Rest, or expected RestDots
;; ret-ty: The expected type of the body of the lambda.
(define/cond-contract (check-clause arg-list rest-id body arg-tys rst ret-ty)
  ((listof identifier?)
   (or/c #f identifier?) syntax? (listof Type?) (or/c #f Rest? RestDots?)
   tc-results/c
   . -> .
   Arrow?)
  (let* ([arg-len (length arg-list)]
         [arg-tys-len (length arg-tys)]
         [extra-arg-count (- arg-len arg-tys-len)]
         [arg-types
          (cond
            [(andmap type-annotation arg-list)
             (get-types arg-list #:default Univ)]
            [(zero? extra-arg-count) arg-tys]
            [(negative? extra-arg-count) (take arg-tys arg-len)]
            [else
             (define tail-tys (match rst
                                [(Rest: rst-tys)
                                 (define rst-len (length rst-tys))
                                 (for/list ([_ (in-range extra-arg-count)]
                                            [rst-t (in-list-cycle rst-tys)])
                                   rst-t)]
                                [_ (for/list ([_ (in-range extra-arg-count)])
                                     -Bottom)]))
             (append arg-tys tail-tys)])])

    ;; Check that the number of formal arguments is valid for the expected type.
    ;; Thus it must be able to accept the number of arguments that the expected
    ;; type has. So we check for two cases: if the function doesn't accept
    ;; enough arguments, or if it requires too many arguments.
    ;; This allows a form like (lambda args body) to have the type (-> Symbol
    ;; Number) with out a rest arg.
    (when (or (and (< arg-len arg-tys-len) (not rest-id))
              (and (> arg-len arg-tys-len) (not rst)))
      (tc-error/delayed (expected-str arg-tys-len rst arg-len rest-id)))

    ;; rst-type - the type of the rest argument in the Arrow type
    ;; rest-body-type - the type the rest argument id has in the body
    ;;                  of the function
    ;; e.g. for
    ;; (: foo (->* () () #:rest String Number))
    ;; (define (foo . rest-strings) ...)
    ;; the caller can provide 0 or more Strings, so the Arrow's
    ;; rest spec would be (make-Rest (list -String))
    ;; and in the body of the function, the rest argument
    ;; identifier (rest-strings) have type (Listof String)
    (define-values (rst-type rest-body-type)
      (cond
        ;; there's not a rest ident... easy
        [(not rest-id) (values #f #f)]
        ;; a dotted rest spec, so the body has a ListDots
        [(RestDots? rst)
         (match-define (RestDots: dty dbound) rst)
         (values rst (make-ListDots dty dbound))]
        ;; the rest-id is dotted?, lets go get its type
        [(dotted? rest-id)
         => (λ (dbound)
              (define ty (extend-tvars (list dbound) (get-type rest-id #:default Univ)))
              (values (make-RestDots ty dbound)
                      (make-ListDots ty dbound)))]
        [else
         ;; otherwise let's get the sequence of types the rest argument would have
         ;; and call it 'rest-types' (i.e. in our above example 'foo', this would
         ;; be (list -String)
         (define rest-types
           (cond
             [(type-annotation rest-id) (list (get-type rest-id #:default Univ))]
             [else
              (match rst
                [#f (list -Bottom)]
                [(? Type? t) (list t)]
                [(Rest: rst-ts) rst-ts]
                [_ (list Univ)])]))
         ;; now that we have the list of types, we need to calculate, based on how many
         ;; positional argument identifiers there are, how the rest should look
         ;; i.e. if our rest was (Num Str)* (i.e. an even length rest arg of numbers
         ;; followed by strings) and there was 1 more positional argument that positional
         ;; domain types, then that extra positional arg would be type Num (i.e. the type
         ;; it gets since its type is coming from the rest type) and the rest id's type
         ;; in the body of the function would (Pair Str (Num Str)*) (i.e. the rest arg
         ;; would _have_ to have a Str in it, and then would have 0 or more Num+Strs
         (cond
           [(= arg-len arg-tys-len)
            (values (make-Rest rest-types)
                    (make-CyclicListof rest-types))]
           ;; some of the args are _in_ the rst arg (i.e. they
           ;; do not have argument names) ...
           [(<= arg-len arg-tys-len)
            (define extra-types (drop arg-tys arg-len))
            (define rst-type (apply Un (append extra-types rest-types)))
            (values (make-Rest (list rst-type))
                    (make-Listof rst-type))]
           ;; there are named args whose type came from the rst argument
           ;; i.e. we need to pull there types out of the rst arg
           [else
            (define rest-remainder (drop rest-types (remainder extra-arg-count
                                                               (length rest-types))))
            (values (make-Rest rest-types)
                    (-Tuple* rest-remainder
                             (make-CyclicListof rest-types)))])]))

    (tc-lambda-body
     arg-list
     arg-types
     #:rest-id+type+body-type (and rst-type (list rest-id rst-type rest-body-type))
     #:expected ret-ty
     body)))

;; typecheck a single lambda, with argument list and body
;; drest-ty and drest-bound are both false or not false
(define/cond-contract (tc/lambda-clause/check f body arg-tys ret-ty rst)
  (-> formals?
      syntax?
      (listof Type?)
      (or/c tc-results/c #f)
      (or/c #f Rest? RestDots?)
      Arrow?)
  (check-clause (formals-positional f)
                (formals-rest f)
                body
                arg-tys
                rst
                ret-ty))

;; typecheck a single opt-lambda clause with argument list and body
(define/cond-contract (tc/opt-lambda-clause arg-list body aux-table)
  (-> (listof identifier?) syntax? free-id-table?
      (listof Arrow?))
  ;; arg-types: Listof[Type?]
  (define arg-types
    (for/list ([a (in-list arg-list)])
      (get-type a #:default (lambda ()
                              (define id (free-id-table-ref aux-table a #f))
                              (cond
                                [id
                                 (define ty (get-type id #:default Univ))
                                 (if (optional-non-immediate-arg-property id)
                                     (Un -Unsafe-Undefined ty)
                                     ty)]
                                [else Univ])))))

  (list (tc-lambda-body arg-list arg-types body)))

;; restrict-to-arity : Arrow? nat -> (or/c #f Arrow?)
;; either produces a new arrow which is a subtype of arr with arity n,
;; or #f is that is not possible
(define (restrict-Arrow-to-arity arrow n)
  (match arrow
    ;; currently does not handle rest arguments
    [(Arrow: args #f '() _)
     #:when (= n (length args))
     arrow]
    [_ #f]))

(define/cond-contract (tc/lambda-clause f body)
  (-> formals? syntax? (listof Arrow?))
  (define aux-table
    (syntax-parse body
      [(b:rebuild-let*) (values (attribute b.mapping))]
      [_ (make-immutable-free-id-table)]))

  (define arg-list (formals-positional f))
  (define rest-id (formals-rest f))

  (define eta-expanded?
    (syntax-parse body
      [(((~literal #%plain-app) fun:id j:id ...)) ;; restricted to ids to avoid re-typechecking
       #:when (equal? (length arg-list)
                      (length (syntax->list #'(j ...))))
       #:when (andmap free-identifier=? arg-list (syntax->list #'(j ...)))
       #'fun]
      [_ #f]))
  
  (cond
    [(and (> (free-id-table-count aux-table) 0) (not rest-id))
     (tc/opt-lambda-clause arg-list body aux-table)]
    [else
     (define arg-types (get-types arg-list #:default (lambda () #f)))
     (define rest-type
       (cond
         ;; Lambda with poly dotted rest argument
         [(and rest-id (dotted? rest-id))
          =>
          (λ (bound)
            (unless (bound-index? bound)
              (if (bound-tvar? bound)
                  (tc-error "Bound on ... type (~a) is not an appropriate type variable" bound)
                  (tc-error/stx rest-id "Bound on ... type (~a) was not in scope" bound)))
            (make-RestDots (extend-tvars (list bound) (get-type rest-id #:default Univ))
                           bound))]
         ;; Lambda with regular rest argument
         [rest-id (match (get-type rest-id #:default Univ)
                    [(? Type? t) (make-Rest (list t))]
                    [(? Rest? rst) rst]
                    [(? RestDots? rst) rst])]
         ;; Lambda with no rest argument
         [else #f]))
     (cond 
      ;; special case for un-annotated eta-expansions
      [(and eta-expanded? (not rest-id) (andmap not arg-types)
            ;; FIXME: should also handle polymorphic types
            ;; but we can't return anything but a (listof arr?) here 
            ;; FIXME: misses optimization opportunities in this code
            (match (tc-expr eta-expanded?)
              [(tc-result1: (Fun: arrows))
               (define possibles
                 (for*/list ([arrow (in-list arrows)]
                             [restricted (in-value (restrict-Arrow-to-arity
                                                    arrow
                                                    (length arg-list)))]
                             #:when restricted)
                   restricted))
               (if (null? possibles)
                   #f
                   possibles)]
              [_ #f]))
       =>
       (lambda (x)
         (register-ignored! (car (syntax-e body)))
         x)]
      [else
       (define rest-body-type
         (match rest-type
           [#f #f]
           [(Rest: ts) (make-CyclicListof ts)]
           [(RestDots: dty dbound) (make-ListDots dty dbound)]))
       (list
        (tc-lambda-body
         arg-list
         (map (λ (v) (or v Univ)) arg-types)
         #:rest-id+type+body-type (and rest-type (list rest-id rest-type rest-body-type))
         body))])]))



;; case-arities
;;     a description of the supported arities for a case-lambda
;;     we have seen thus far while checking a case-lambda (recall
;;     that, for Function types and type checking purposes, all
;;     functions are case-lambdas)
;; fixed-arities : (listof natural?)
;;     supported unique options so far for fixed argument counts,
;;     where for each element n, n < rest-pos, and the list should
;;     not contain duplicates
;;     (NOTE: once we encounter a rest arg at position rest-pos, we
;;            _remove_ arity counts that the rest encompasses (i.e.
;;            when n >= rest-pos) -- see example below of
;;            checking a case-lambda)
;; rest-pos : (or/c natural? +inf.0)
;;     at what position would an argument be in the rest argument
;;
;; We construct these specs _while_ we are parsing and checking
;; case-lambdas to help us know if a clause is dead code,
;; which Arrow types we should type check a particular case-lambda
;; clause at, etc
;;
;; e.g. after each step of looking at the below case-lambda,
;; we would have the following case-arity-spec (i.e. after
;; using `add-to-arities` to update the case-arities):
;; (case-lambda
;;  [(x) ...]          ; ==> (case-arities '(1)     +inf.0)
;;  [(x y) ...]        ; ==> (case-arities '(1 2)   +inf.0)
;;  [(x y z) ...]      ; ==> (case-arities '(1 2 3) +inf.0)
;;  [(x y . rst) ...]  ; ==> (case-arities '(1 2)   2)
;;  [l ...])           ; ==> (case-arities '()      0)
(struct case-arities (fixed-arities rest-pos) #:transparent)

;; initially, we have seen no fixed arities and it is impossible for
;; an argument to be in a rest argument from a previous clause
(define initial-case-arities (case-arities '() +inf.0))

;; Adds the arity described by formals 'f' to the arities
;; described by 'arities'. See above example near `case-arities`
;; definition.
(define/cond-contract (add-to-arities arities f)
  (-> case-arities? formals? case-arities?)
  (match* (arities f)
    [((case-arities fixed-arities rest-pos)
      (formals positional rst _))
     (define arity (length positional))
     (define new-rest-pos
       (if rst (min rest-pos arity) rest-pos))
     (define new-fixed-arities
       (cond
         [(eqv? +inf.0 new-rest-pos) (cons arity fixed-arities)]
         [else (for/list ([i (in-list (cons arity fixed-arities))]
                          #:when (< i new-rest-pos))
                 i)]))
     (case-arities new-fixed-arities new-rest-pos)]))


;; Determines if the given formals would be
;; covered by a supported arity in arities
(define/cond-contract (in-arities? arities f-or-arrow)
  (-> case-arities? (or/c formals? Arrow?) boolean?)
  (match* (arities f-or-arrow)
    [((case-arities fixed-arities rest-pos)
      (or (formals (app length arity) rst _)
          (Arrow:  (app length arity) rst _ _)))
     (or (>= arity rest-pos)
         (and (not rst) (memv arity fixed-arities) #t))]))



;; Returns a list of Arrows where the list contains all the valid Arrows
;; from 'arrows' that could apply to a clause with formals 'f', given we
;; have already seen case-arities 'seen'.
(define/cond-contract (arrows-matching-seen+formals arrows seen f)
  (-> (listof Arrow?) case-arities? formals? (listof Arrow?))
  (match-define (formals formals-positionals formals-rest? _) f)
  (define pos-count (length formals-positionals))
  (for*/list ([arrow (in-list arrows)]
              [dom (in-value (Arrow-dom arrow))]
              [rst (in-value (Arrow-rst arrow))]
              #:unless (in-arities? seen arrow)
              #:when (cond
                       [formals-rest?
                        (or (Rest? rst) (>= (length dom) pos-count))]
                       [rst (<= (length dom) pos-count)]
                       [else (= (length dom) pos-count)]))
    arrow))



;; For each clause (i.e. each elem in formals+bodies) we figure out which
;; of the expected arrows it needs to type check at and which clauses
;; are dead code.
;;
;; Returns the association list mapping clauses to the arrows they need
;; to type check at.
(define/cond-contract (create-to-check-list formals+bodies expected-arrows)
  (-> (listof (cons/c formals? syntax?))
      (listof Arrow?)
      (listof (cons/c (cons/c formals? syntax?)
                      (listof Arrow?))))
  ;; arities we have seen so far while checking case-lambda clauses
  (define seen initial-case-arities)
  (for*/list ([f+b (in-list formals+bodies)]
              [clause-formals (in-value (car f+b))]
              [clause-body (in-value (cdr f+b))])
    (define matching-arrows
      (arrows-matching-seen+formals expected-arrows
                                    seen
                                    clause-formals))
    (when (or (in-arities? seen clause-formals)
              (null? matching-arrows))
      (warn-unreachable clause-body)
      (add-dead-lambda-branch (formals-syntax clause-formals)))
    (set! seen (add-to-arities seen clause-formals))
    (cons f+b matching-arrows)))


;; formals+bodies  : formals and bodies to check
;; expected-arrows : expected arrow types for the overall case-lambda
;; orig-arrows     : an association list recording if any formals and bodies
;;                   have _already_ been checked at a certain Arrow type
(define/cond-contract
  (check-mono-lambda/expected formals+bodies expected-arrows orig-arrows)
  (-> (listof (cons/c formals? syntax?))
      (listof Arrow?)
      (listof (cons/c (cons/c formals? syntax?)
                      (listof Arrow?)))
      (listof Arrow?))


  (define to-check-list (create-to-check-list formals+bodies expected-arrows))

  (cond
    [(and (andmap (λ (f+b+arrows) (null? (cdr f+b+arrows)))
                  to-check-list)
          ;; If the empty function is expected, then don't error out
          (not (null? expected-arrows)))
     ;; TODO improve error message.
     (tc-error/expr #:return (list (-Arrow null -Bottom #:rest Univ))
                    "Expected a function of type ~a, but got a function with the wrong arity"
                    (make-Fun expected-arrows))]
    [else
     (for*/list ([(f+b arrows-to-check-against) (in-assoc to-check-list)]
                 [clause-formals (in-value (car f+b))]
                 [clause-body (in-value (cdr f+b))]
                 [orig-arrows (in-value (assoc-ref orig-arrows f+b '()))]
                 [arrow (in-list arrows-to-check-against)])
       ;; NOTE!!! checking clauses against all applicable arrows is sound, but
       ;; less complete than actually intersecting all of the arrow types and
       ;; then checking clauses against the result
       (match arrow
         ;; if this clause has an orig-arrow, we already checked it once and that
         ;; was it's arrow type -- we don't want to check it again at the same arrow
         [_ #:when (member arrow orig-arrows) arrow]
         [(Arrow: dom rst '() rng)
          (define expected
            (values->tc-results rng (formals->objects clause-formals)))
          (tc/lambda-clause/check
           clause-formals clause-body dom expected rst)]))]))


;; typecheck a sequence of case-lambda clauses
(define/cond-contract (tc/mono-lambda formals+bodies expected)
  (-> (listof (cons/c formals? syntax?))
      (or/c #f tc-results/c)
      (listof Arrow?))
  (define expected-arrows
    (match expected
      [(tc-result1: t)
       (define resolved (resolve t))
       (match resolved
         [(Fun: arrows)
          #:when (for/and ([arr (in-list arrows)])
                   (null? (Arrow-kws arr)))
          arrows]
         [_ #f])]
      [_ #f]))
  (cond
    [expected-arrows
     ;; if we have expected Arrows, proceed with checking against them
     (check-mono-lambda/expected formals+bodies expected-arrows '())]
    [else
     ;; if we don't have expected Arrows, we may need to re-check some
     ;; of the bodies against Arrow types derived while checking the
     ;; bodies for soundness sake, so first we will check their bodies
     ;; with no expected type and then use check-mono-lambda/expected
     ;; to perform any re-checking that is needed

     ;; arities we have seen so far while checking case-lambda clauses
     (define seen initial-case-arities)
     ;; clauses that are unreachable because of the formals and ordering
     ;; of the case-lambda clauses
     (define unreachable-clauses '())
     ;; now we typecheck the bodies, recording their arrows (resulting-arrows)
     ;; and the mapping of which formals+body produced which Arrow (already-checked),
     ;; all while updating which arities we have seen and which, if any, case-lambda
     ;; clauses are in fact unreachable
     (define-values (resulting-arrowss already-checked)
       (for*/lists (_1 _2)
         ([f+b (in-list formals+bodies)]
          [f (in-value (car f+b))]
          [b (in-value (cdr f+b))]
          #:unless  (let ([unreachable? (in-arities? seen f)])
                      (when unreachable?
                        (warn-unreachable b)
                        (add-dead-lambda-branch (formals-syntax f))
                        (set! unreachable-clauses
                              (cons f+b unreachable-clauses)))
                      unreachable?))
         (set! seen (add-to-arities seen f))
         (define resulting-arrow (tc/lambda-clause f b))
         (values resulting-arrow
                 (cons f+b resulting-arrow))))

     (define resulting-arrows (apply append resulting-arrowss))

     ;; if there was more than one live case-lambda clause, we may need
     ;; to recheck some clauses against some of the arrows generated
     ;; during checking for soundness sake,
     ;; e.g.
     ;; if we naively check (case-lambda
     ;;                       [([x : Num] . [rst : Num *]) x]
     ;;                       [[rst : Num *] 0]
     ;; we get (case-> (-> Num Num * Num)
     ;;                (-> Num * Zero))
     ;; which is unsound (i.e. we can upcast an intersection to either
     ;; type, namely in this case to (-> Num * Zero), and then call 
     ;; it as the identity function on any number, which does not
     ;; always produce the constant 0). In other words, our `case->`
     ;; is really an UNORDERED intersection that we just don't work
     ;; super hard to check function application with, it is not
     ;; truly an ordered intersection, and thus if some function `f`
     ;; has type `A ∧ B` it must be checked at both `A` and `B`.
     (cond
       [(> (- (length formals+bodies)
              (length unreachable-clauses))
           1)
        (check-mono-lambda/expected (remove* unreachable-clauses
                                             formals+bodies)
                                    resulting-arrows
                                    already-checked)]
       [else
        resulting-arrows])]))

(define (tc/dep-lambda formalss-stx bodies-stx dep-fun-ty)
  (parameterize ([with-refinements? #t])
    (match-define (DepFun: raw-dom raw-pre raw-rng) dep-fun-ty)
    (define formalss (stx-map make-formals formalss-stx))
    (define bodies (syntax->list bodies-stx))
    (match* (formalss bodies)
      [((list fs) (list body))
       (cond
         [(not (= (length (formals-positional fs))
                  (length raw-dom)))
          (tc-error/expr #:return dep-fun-ty
                         (format "Expected ~a positional arguments, given ~a."
                                 (length raw-dom)
                                 (length (formals-positional fs))))]
         [(formals-rest fs)
          (tc-error/expr #:return dep-fun-ty
                         "Dependent functions do not currently support rest arguments.")]
         [else
          (define arg-names (formals-positional fs))
          (define dom (for/list ([d (in-list raw-dom)])
                        (instantiate-obj d arg-names)))
          (define pre (instantiate-obj raw-pre arg-names))
          (with-naively-extended-lexical-env
              [#:identifiers arg-names
               #:types dom
               #:props (list pre)]
            (tc-body/check body (values->tc-results raw-rng (map -id-path arg-names))))
          dep-fun-ty])]
      [(fs bs)
       (tc-error/expr #:return dep-fun-ty
                      "Dependent functions must have a single arity.")])))

(define (tc/mono-lambda/type formalss bodies expected)
  (match expected
    [(tc-result1:(? DepFun? dep-fun-ty))
     (tc/dep-lambda formalss bodies dep-fun-ty)]
    [_ (make-Fun
        (tc/mono-lambda
         (for/list ([f (in-syntax formalss)]
                    [b (in-syntax bodies)])
           (cons (make-formals f) b))
         expected))]))

(define (plambda-prop stx)
  (define d (plambda-property stx))
  (and d (car (flatten d))))

(define (has-poly-annotation? form)
  (or (plambda-prop form) (pair? (lookup-scoped-tvar-layer form))))

(define (remove-poly-layer tvarss)
  (filter pair? (map rest tvarss)))

(define (get-poly-layer tvarss)
  (map car tvarss))

(define (get-poly-tvarss form)
  (let ([plambda-tvars
          (let ([p (plambda-prop form)])
            (match (and p (map syntax-e (syntax->list p)))
              [#f #f]
              [(list var ... dvar '...)
               (list (list var dvar))]
              [(list id ...)
               (list id)]))]
        [scoped-tvarss
          (for/list ((tvarss (in-list (lookup-scoped-tvar-layer form))))
            (for/list ((tvar (in-list tvarss)))
              (match tvar
                [(list (list v ...) dotted-v)
                 (list (map syntax-e v) (syntax-e dotted-v))]
                [(list v ...) (map syntax-e v)])))])
    (if plambda-tvars
        (cons plambda-tvars scoped-tvarss)
        scoped-tvarss)))


;; tc/plambda syntax tvarss-list syntax-list syntax-list type -> Poly
;; formals and bodies must by syntax-lists
(define/cond-contract (tc/plambda form tvarss-list formals bodies expected)
  (syntax? (listof list?) syntax? syntax? (or/c tc-results/c #f) . -> . Type?)
  (define/cond-contract (maybe-loop form formals bodies expected)
    (syntax? syntax? syntax? (or/c tc-results/c #f) . -> . Type?)
    (match expected
      [(tc-result1: (app resolve (or (? Poly?) (? PolyDots?) (? PolyRow?))))
       (tc/plambda form (remove-poly-layer tvarss-list) formals bodies expected)]
      [_
        (define remaining-layers (remove-poly-layer tvarss-list))
        (if (null? remaining-layers)
            (tc/mono-lambda/type formals bodies expected)
            (tc/plambda form remaining-layers formals bodies expected))]))
  ;; check the bodies appropriately
  ;; and make both annotated and declared type variables point to the
  ;; same actual type variables (the fresh names)
  (define (extend-and-loop form ns formals bodies expected)
    (let loop ([tvarss tvarss])
      (match tvarss
        [(list) (maybe-loop form formals bodies expected)]
        [(cons (list (list tvars ...) dotted) rest-tvarss)
         (extend-indexes dotted
            (extend-tvars/new tvars ns
              (loop rest-tvarss)))]
        [(cons tvars rest-tvarss)
         (extend-tvars/new tvars ns
           (loop rest-tvarss))])))
  (define tvarss (get-poly-layer tvarss-list))

  (match expected
    [(tc-result1: (app resolve (and t (Poly-fresh: ns fresh-ns expected*))))
     ;; make sure the declared and annotated type variable arities match up
     ;; with the expected type variable arity
     (for ([tvars (in-list tvarss)])
       (when (and (pair? tvars) (list? (car tvars)))
         (tc-error
          "Expected a polymorphic function without ..., but given function/annotation had ..."))
       (unless (= (length tvars) (length fresh-ns))
         (tc-error "Expected ~a type variables, but given ~a"
                   (length fresh-ns) (length tvars))))
     (make-Poly #:original-names ns fresh-ns (extend-and-loop form fresh-ns formals bodies (ret expected*)))]
    [(tc-result1: (app resolve (and t (PolyDots-names: (list ns ... dvar) expected*))))
     ;; make sure the declared and annotated type variable arities match up
     ;; with the expected type variable arity
     (for ((tvars (in-list tvarss)))
       (match tvars
         [(list (list vars ...) dotted)
          (unless (= (length vars) (length ns))
            (tc-error "Expected ~a non-dotted type variables, but given ~a"
                      (length ns) (length vars)))]
         [else
          (tc-error "Expected a polymorphic function with ..., but function/annotation had no ...")]))
     (make-PolyDots (append ns (list dvar)) (extend-and-loop form ns formals bodies (ret expected*)))]
    [(tc-result1: (app resolve (and t (PolyRow-fresh: ns fresh-ns constraints expected*))))
     (for ((tvars (in-list tvarss)))
       (when (and (pair? tvars) (list? (car tvars)))
         (tc-error
          "Expected a polymorphic function without ..., but given function/annotation had ..."))
       (unless (= (length tvars) 1)
         (tc-error "Expected ~a type variable, but given ~a"
                   1 (length tvars))))
     (make-PolyRow
      #:original-names ns
      fresh-ns
      constraints
      (extend-and-loop form fresh-ns
                       formals bodies (ret expected*)))]
    [_
     (define lengths
       (remove-duplicates
        (for/list ([tvars (in-list tvarss)])
          (match tvars
            [(list (list vars ...) dotted)
             (length vars)]
            [(list vars ...)
             (length vars)]))))
     (define dots
       (remove-duplicates
        (for/list ([tvars (in-list tvarss)])
          (match tvars
            [(list (list vars ...) dotted) #t]
            [(list vars ...) #f]))))
     (unless (= 1 (length lengths))
       (tc-error "Expected annotations to have the same number of type variables, but given ~a"
                 lengths))
     (unless (= 1 (length dots))
       (tc-error "Expected annotations to all have ... or none to have ..., but given both"))
     (define dotted (and (car dots) (second (car tvarss))))
     (define ns (build-list (car lengths) (lambda (_) (gensym))))
     (define results (extend-and-loop form ns formals bodies expected))
     (if dotted
         (make-PolyDots (append ns (list dotted)) results)
         (make-Poly #:original-names (car tvarss) ns results))]))

;; typecheck a sequence of case-lambda clauses, which is possibly polymorphic
;; tc/lambda : syntax syntax-list syntax-list (or/c tc-results #f) -> tc-results
(define (tc/lambda form formals bodies expected)
  (if (or (has-poly-annotation? form)
          (match expected
            [(tc-result1: (app resolve t)) (or (Poly? t) (PolyDots? t) (PolyRow? t))]
            [_ #f]))
      (ret (tc/plambda form (get-poly-tvarss form) formals bodies expected) -true-propset)
      (ret (tc/mono-lambda/type formals bodies expected) -true-propset)))

;; formals : the formal arguments to the loop
;; body : a block containing the body of the loop
;; name : the name of the loop
;; args : the types of the actual arguments to the loop
;; ret : the expected return type of the whole expression
;; Returns both the tc-results of the function and of the body
(define (tc/rec-lambda/check formals* body name args return)
  (define formals (syntax->list formals*))
  (define ft (t:->* args (tc-results->values return)))
  (define names (cons name formals))
  (with-extended-lexical-env
    [#:identifiers (cons name formals)
     #:types (cons ft args)]
    (values
     (erase-identifiers (ret ft) names)
     (erase-identifiers (tc-body/check body return) names))))
