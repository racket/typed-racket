#lang racket/base

;; Contract generation for Typed Racket

(require
 "../utils/utils.rkt"
 syntax/parse
 "../rep/type-rep.rkt"
 "../rep/prop-rep.rkt"
 "../rep/object-rep.rkt"
 "../rep/fme-utils.rkt"

 "../utils/tc-utils.rkt"
 "../utils/prefab.rkt"
 "../utils/identifier.rkt"

 "../env/type-name-env.rkt"
 "../env/row-constraint-env.rkt"
 "../env/lexical-env.rkt"
 "../env/type-constr-env.rkt"

 "../rep/core-rep.rkt"
 "../rep/rep-utils.rkt"
 "../rep/free-ids.rkt"
 "../rep/type-mask.rkt"
 "../rep/values-rep.rkt"
 "../rep/base-types.rkt"
 "../rep/numeric-base-types.rkt"
 "../rep/free-variance.rkt"
 "../types/resolve.rkt"
 "../types/utils.rkt"
 "../types/printer.rkt"
 "../types/match-expanders.rkt"
 "../types/union.rkt"
 "../types/subtype.rkt"
 (prefix-in t: (combine-in "../types/abbrev.rkt"
                           "../types/numeric-tower.rkt"
                           "../types/subtype.rkt"))
 "parse-type.rkt"
 "syntax-properties.rkt"
 racket/match racket/syntax racket/list
 racket/format
 racket/string
 racket/set
 racket/treelist
 syntax/flatten-begin
 (only-in "../types/abbrev.rkt" -Bottom -Boolean)
 "../static-contracts/instantiate.rkt"
 "../static-contracts/structures.rkt"
 "../static-contracts/combinators.rkt"
 "../static-contracts/constraints.rkt"
 (only-in (submod typed-racket/static-contracts/instantiate internals) compute-constraints)
 ;; TODO make this from contract-req
 (prefix-in c: racket/contract)
 (contract-req)
 (only-in racket/unsafe/undefined unsafe-undefined)
 (for-syntax racket/base)
 (for-template racket/base racket/contract "../utils/any-wrap.rkt" "../utils/shallow-contract.rkt"))

(provide
  (c:contract-out
    [type->static-contract
      (c:parametric->/c (a) ((Type? (c:-> #:reason (c:or/c #f string?) a))
                             (#:typed-side boolean?
                              #:enforcement-mode type-enforcement-mode?)
                             . c:->* . (c:or/c a static-contract?)))]))

(provide change-contract-fixups
         change-provide-fixups
         any-wrap/sc
         extra-requires
         include-extra-requires?)

;; submod for testing
(module* test-exports #f (provide type->contract has-contract-def-property? make-procedure-arity-flat/sc))


(define num-existentials (make-parameter 0))
;; has-contrat-def-property? : Syntax -> Boolean
(define (has-contract-def-property? stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(define-values (_) e)
     (and (contract-def-property #'e)
          #t)]
    [_ #f]))

;; type : (syntaxof Type?)
;; flat? : boolean?
;; maker? : boolean?
;; typed-side : (or/c 'untyped 'typed)
;; te-mode : type-enforcement-mode?
(struct contract-def (type flat? maker? typed-side te-mode) #:prefab)

;; get-contract-def-property : Syntax -> (U False Contract-Def)
;; Checks if the given syntax needs to be fixed up for contract generation
;; and if yes it returns the information stored in the property
(define (get-contract-def-property stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(define-values (_) e)
     (and (contract-def-property #'e)
          ((contract-def-property #'e)))]
    [_ #f]))

;; type->contract-fail : Syntax Type #:ctc-str String
;;                       -> #:reason (Option String) -> Void
;; Curried function that produces a function to report
;; type->contract failures
(define ((type->contract-fail to-check to-report
                              #:ctc-str [ctc-str "contract"])
         #:reason [reason #f])
  (tc-error/stx
   to-report
   (~a "Type ~a could not be converted to a "
       ctc-str
       (if reason (~a ": " reason) "."))
   to-check))

;; The cache is used to share contract definitions in the generated code
;; across multiple calls to type->contract.
;; This saves computation time and zo space for excessively large types
;; (such as mutually recursive class types).
(define (generate-contract-def stx cache)
  (define prop (get-contract-def-property stx))
  (match-define (contract-def type-stx flat? maker? typed-side te-mode) prop)
  (define *typ (if type-stx (parse-type type-stx) t:-Dead-Code))
  (define kind (if (and type-stx flat?) 'flat 'impersonator))
  (syntax-parse stx #:literals (define-values)
    [(define-values (n) _)
     (define typ
       (cond [maker?
              (match (lookup-type-name (Name-id *typ))
                [(Poly-names: names body)
                 (make-Poly names
                   ((map fld-t (Struct-flds body)) #f . t:->* . *typ))]
                [ty
                 ((map fld-t (Struct-flds ty)) #f . t:->* . *typ)])]
             [else *typ]))
     (match-define (list defs ctc)
       (type->contract
        typ
        ;; this value is from the typed side (require/typed, make-predicate, etc)
        ;; unless it's used for with-type
        #:typed-side (from-typed? typed-side)
        #:enforcement-mode te-mode
        #:kind kind
        #:cache cache
        (type->contract-fail
         typ type-stx
         #:ctc-str (if flat? "predicate" "contract"))))
     (ignore ; should be ignored by the optimizer
      (quasisyntax/loc stx
        (begin #,@defs (define-values (n) #,ctc))))]
    [_ (int-err "should never happen - not a define-values: ~a"
                (syntax->datum stx))]))

;; Generate a contract for a TR provide form
(define (generate-contract-def/provide stx cache)
  (match-define (list type untyped-id orig-id blame-id te-mode)
    (contract-def/provide-property stx))
  (define failure-reason #f)
  (define result
    (type->contract type
                    #:typed-side (if (eq? 'deep (current-type-enforcement-mode)) #t 'both)
                    #:kind 'impersonator
                    #:cache cache
                    #:enforcement-mode te-mode
                    ;; FIXME: get rid of this interface, make it functional
                    (λ (#:reason [reason #f]) (set! failure-reason reason))))
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(define-values (ctc-id) _)
     ;; no need for ignore, the optimizer doesn't run on this code
     (cond [failure-reason
            #`(define-syntax (#,untyped-id stx)
                (tc-error/fields #:stx stx
                                 "could not convert type to a contract"
                                 #:more #,failure-reason
                                 "identifier" #,(symbol->string (syntax-e orig-id))
                                 "type" #,(pretty-format-rep type #:indent 8)))]
           [else
            (match-define (list defs ctc) result)
            (define maybe-inline-val
              (should-inline-contract?/cache ctc cache))
            #`(begin #,@defs
                     #,@(if maybe-inline-val
                            null
                            (list #`(define-values (ctc-id) #,ctc)))
                     (define-module-boundary-contract #,untyped-id
                       #,orig-id
                       #,(or maybe-inline-val #'ctc-id)
                       #:pos-source #,blame-id
                       #:srcloc (vector (quote #,(syntax-source orig-id))
                                        #,(syntax-line orig-id)
                                        #,(syntax-column orig-id)
                                        #,(syntax-position orig-id)
                                        #,(syntax-span orig-id))))])]))

;; Syntax (Dict Static-Contract (Cons Id Syntax)) -> (Option Syntax)
;; A helper for generate-contract-def/provide that helps inline contract
;; expressions when needed to cooperate with the contract system's optimizations
(define (should-inline-contract?/cache ctc-stx cache)
  (and (identifier? ctc-stx)
       (let ([match? (assoc ctc-stx (hash-values cache) free-identifier=?)])
         (and match?
              (should-inline-contract? (cdr match?))
              (cdr match?)))))

;; The below requires are needed since they provide identifiers that
;; may appear in the residual program.

;; TODO: It would be better to have individual contracts specify which
;; modules should be required, but for now this is just all of them.
(define extra-requires
  #'(require
      (submod typed-racket/private/type-contract predicates)
      typed-racket/utils/utils
      (for-syntax typed-racket/utils/utils)
      typed-racket/utils/any-wrap typed-racket/utils/struct-type-c
      typed-racket/utils/prefab-c
      typed-racket/utils/opaque-object
      typed-racket/utils/evt-contract
      typed-racket/utils/hash-contract
      typed-racket/utils/vector-contract
      typed-racket/utils/sealing-contract
      typed-racket/utils/promise-not-name-contract
      typed-racket/utils/simple-result-arrow
      typed-racket/utils/eq-contract
      racket/sequence
      racket/contract/parametric
      typed-racket/utils/shallow-contract))

;; Should the above requires be included in the output?
;;   This box is only used for contracts generated for `require/typed`
;;   and `cast`, contracts for `provides go into the `#%contract-defs`
;;   submodule, which always has the above `require`s.
(define include-extra-requires? (box #f))

(define (change-contract-fixups forms [ctc-cache (make-hash)])
  (with-new-name-tables
   (for/list ((e (in-list forms)))
     (cond
       [(not (has-contract-def-property? e)) e]
       [else
        (set-box! include-extra-requires? #t)
        (generate-contract-def e ctc-cache)]))))

;; TODO: These are probably all in a specific place, which could avoid
;;       the big traversal
(define (change-provide-fixups forms [ctc-cache (make-hash)])
  (with-new-name-tables
   (for/list ([form (in-list forms)])
     (syntax-parse form #:literal-sets (kernel-literals)
       [_
        #:when (contract-def/provide-property form)
        (generate-contract-def/provide form ctc-cache)]
       [(module* name #f forms ...)
        (quasisyntax/loc form
          (module* name #f
            #,@(change-provide-fixups (syntax->list #'(forms ...))
                                      ctc-cache)))]
       [((~literal #%plain-module-begin) forms ...)
        (quasisyntax/loc form
          (#%plain-module-begin
           #,@(change-provide-fixups (flatten-all-begins #'(begin forms ...))
                                     ctc-cache)))]
       [_ form]))))

;; get-max-contract-kind
;; static-contract -> (or/c 'flat 'chaperone 'impersonator)
;; recurse into a contract finding the max
;; kind (e.g. flat < chaperone < impersonator)
(define (get-max-contract-kind sc)
  (kind-max-max (contract-restrict-value (compute-constraints sc 'impersonator))))

;; To avoid misspellings
(define impersonator-sym 'impersonator)
(define chaperone-sym 'chaperone)
(define flat-sym 'flat)

(define (contract-kind-max i . args)
  (define (contract-kind-max2 x y)
    (cond
      ((equal? flat-sym x) y)
      ((equal? flat-sym y) x)
      ((equal? chaperone-sym x) y)
      ((equal? chaperone-sym y) x)
      (else impersonator-sym)))
  (for/fold ((acc i)) ((v (in-list args)))
    (contract-kind-max2 v acc)))

(define (contract-kind-min i . args)
  (define (contract-kind-min2 x y)
    (cond
      ((equal? flat-sym x) x)
      ((equal? flat-sym y) y)
      ((equal? chaperone-sym x) x)
      ((equal? chaperone-sym y) y)
      (else impersonator-sym)))
  (for/fold ((acc i)) ((v (in-list args)))
    (contract-kind-min2 v acc)))


(define (contract-kind->keyword sym)
  (string->keyword (symbol->string sym)))

(define typed-side?-str "(or/c 'typed 'untyped 'both)")

(define (from-typed? side)
  (case side
   [(typed both) #t]
   [(untyped) #f]
   [else (raise-argument-error 'from-typed? typed-side?-str side)]))

(define (from-untyped? side)
  (case side
   [(untyped both) #t]
   [(typed) #f]
   [else (raise-argument-error 'from-untyped? typed-side?-str side)]))

(define (flip-side side)
  (case side
   [(typed) 'untyped]
   [(untyped) 'typed]
   [(both) 'both]
   [else (raise-argument-error 'flip-side typed-side?-str side)]))

;; type->contract : Type Procedure
;;                  #:typed-side (U 'both Boolean) #:kind Symbol #:cache Hash
;;                  -> (U Any (List (Listof Syntax) Syntax))
(define (type->contract ty init-fail
                        #:typed-side [typed-side #t]
                        #:kind [pre-kind 'impersonator]
                        #:cache [cache (make-hash)]
                        #:enforcement-mode [te-mode (current-type-enforcement-mode)])
  (let/ec escape
    (define (fail #:reason [reason #f]) (escape (init-fail #:reason reason)))
    (define sc
      (type->static-contract ty fail
                             #:typed-side typed-side
                             #:enforcement-mode te-mode))
    (define kind (if (eq? deep te-mode) pre-kind 'flat))
    (define-values [trust-pos? trust-neg?]
      (if (eq? typed-side 'both)
        (values #f #f)
        (values typed-side (not typed-side))))
    (instantiate/optimize sc fail kind
      #:cache cache
      #:trusted-positive trust-pos?
      #:trusted-negative trust-neg?)))

(define any-wrap/sc (chaperone/sc #'any-wrap/c))

(define (no-duplicates l)
  (= (length l) (length (remove-duplicates l))))

(struct triple (untyped typed both))
(define (triple-lookup trip side)
  (case side
    ((untyped) (triple-untyped trip))
    ((typed) (triple-typed trip))
    ((both) (triple-both trip))
    (else (raise-argument-error 'triple-lookup typed-side?-str 1 trip side))))
(define (same sc)
  (triple sc sc sc))


(define (type->static-contract type init-fail
                               #:typed-side [typed-side #t]
                               #:enforcement-mode [te-mode (current-type-enforcement-mode)])
  (case te-mode
    [(shallow)
     (type->static-contract/shallow type #:typed-side typed-side)]
    [(optional)
     any/sc]
    [else ; (deep #f)
     (type->static-contract/deep type init-fail #:typed-side typed-side)]))

(define (type->static-contract/deep type init-fail
                                       #:typed-side [typed-side #t])
  (let/ec return
    (define (fail #:reason reason) (return (init-fail #:reason reason)))
    (let loop ([type type] [typed-side (if typed-side 'typed 'untyped)] [recursive-values (hash)])
      (define (t->sc t #:recursive-values (recursive-values recursive-values))
        (loop t typed-side recursive-values))
      (define (t->sc/neg t #:recursive-values (recursive-values recursive-values))
        (loop t (flip-side typed-side) recursive-values))
      (define (t->sc/both t #:recursive-values (recursive-values recursive-values))
        (loop t 'both recursive-values))
      (define (t->sc/fun t #:maybe-existential [opt-exi #f]) (t->sc/function t fail typed-side recursive-values loop #f #:maybe-existential opt-exi))
      (define (t->sc/meth t) (t->sc/method t fail typed-side recursive-values loop))

      (define (struct->recursive-sc name-base key flds sc-ctor)
        (define key* (generate-temporary name-base))
        (define rv (hash-set recursive-values
                             key
                             (recursive-sc-use key*)))
        (define ftsc (for/list ([ft (in-list flds)])
                       (t->sc ft #:recursive-values rv)))
        (recursive-sc (list key*) (list (sc-ctor ftsc))
                      (recursive-sc-use key*)))

      (define (prop->sc p)
        (match p
          [(TypeProp: o (app t->sc tc))
           (cond
             [(not (equal? flat-sym (get-max-contract-kind tc)))
              (fail #:reason "proposition contract generation not supported for non-flat types")]
             [else (is-flat-type/sc (obj->sc o) tc)])]
          [(NotTypeProp: o (app t->sc tc))
           (cond
             [(not (equal? flat-sym (get-max-contract-kind tc)))
              (fail #:reason "proposition contract generation not supported for non-flat types")]
             [else (not-flat-type/sc (obj->sc o) tc)])]
          [(LeqProp: (app obj->sc lhs) (app obj->sc rhs))
           (leq/sc lhs rhs)]
          ;; TODO: check for (<= x y) and (<= y x)
          ;; and generate and = instead of two <=
          [(AndProp: ps)
           (and-prop/sc (map prop->sc ps))]
          [(OrProp: ps)
           (or-prop/sc (map prop->sc ps))]))
      (define (only-untyped sc)
        (if (from-typed? typed-side)
            (and/sc sc any-wrap/sc)
            sc))
      (match type
       ;; Applications of implicit recursive type aliases
       ;;
       ;; We special case this rather than just resorting to standard
       ;; App resolution (see case below) because the resolution process
       ;; will make type->static-contract infinite loop.
       [(App: (Name: name _ #f) _)
        ;; Key with (cons name 'app) instead of just name because the
        ;; application of the Name is not necessarily the same as the
        ;; Name type alone
        (cond
          ;; when constr is a built-in or non-recursive user-defined type
          ;; constructor, don't generate a recursive static contract
          ;; for the resulting type.
          [(simple-type-constructor? name)
           (t->sc (resolve-once type))]
          [(hash-ref recursive-values (cons name 'app) #f)]
          [else
           (define name* (generate-temporary name))
           (recursive-sc (list name*)
                         (list
                          (t->sc (resolve-once type)
                                 #:recursive-values
                                 (hash-set recursive-values
                                           (cons name 'app)
                                           (recursive-sc-use name*))))
                         (recursive-sc-use name*))])]
       ;; Implicit recursive aliases
       [(Name: name-id args #f)
        (cond [;; recursive references are looked up in a special table
               ;; that's handled differently by sc instantiation
               (lookup-name-sc type typed-side)]
              [else
               (define rv recursive-values)
               ;; FIXME: need special treatment for type constructors
               (define resolved-name (resolve-once type))
               (register-name-sc type
                                 (λ () (loop resolved-name 'untyped rv))
                                 (λ () (loop resolved-name 'typed rv))
                                 (λ () (loop resolved-name 'both rv)))
               (lookup-name-sc type typed-side)])]
       ;; Ordinary type applications or struct type names, just resolve
       [(or (App: _ _) (Name/struct:)) (t->sc (resolve-once type))]
       [(Univ:) (only-untyped any/sc)]
       [(Bottom:) (or/sc)]
       [(Listof: elem-ty) (listof/sc (t->sc elem-ty))]
       ;; This comes before Base-ctc to use the Value-style logic
       ;; for the singleton base types (e.g. -Null, 1, etc)
       [(Val-able: v)
        (cond
          [(eof-object? v)
           (flat/sc #'eof-object?)]
          [(void? v)
           (flat/sc #'void?)]
          [(and (c:flat-contract? v)
                ;; numbers used as contracts compare with =, but TR
                ;; requires an equal? check
                (not (number? v))
                ;; regexps don't match themselves when used as contracts
                (not (or (regexp? v) (byte-regexp? v))))
            (flat/sc #`(quote #,v))]
          [else
           (flat/sc #`(flat-named-contract '#,v (lambda (x) (equal? x '#,v))))])]
       [(Base-name/contract: sym ctc) (flat/sc ctc)]
       [(Distinction: _ _ t) ; from define-new-subtype
        (t->sc t)]
       [(Refinement: par p?)
        (and/sc (t->sc par) (flat/sc p?))]
       [(BaseUnion: bbits nbits)
        (define numeric (make-BaseUnion #b0 nbits))
        (define other-scs (map t->sc (bbits->base-types bbits)))
        (define numeric-sc (numeric-type->static-contract numeric))
        (if numeric-sc
            (apply or/sc numeric-sc other-scs)
            (apply or/sc (append other-scs (map t->sc (nbits->base-types nbits)))))]
       [(? Union? t)
        (match (normalize-type t)
          [(Union-all-flat: elems)
           ;; merge hash and vector types if they have equal components
           (let*-values ([(hash-tys elems) (partition hash-type? elems)]
                         [(vec-tys elems) (partition vector-type? elems)]
                         [(hvec-tys elems) (partition heterogeneous-vector-type? elems)])
             (define scs
               (append
                 (merge-hash-types hash-tys t->sc (only-untyped hash?/sc))
                 (merge-vector-types vec-tys t->sc (only-untyped vector?/sc))
                 (merge-heterogeneous-vector-types hvec-tys t->sc)
                 (map t->sc elems)))
             ;; bg: avoid singleton or/sc's, to make unit testing easier
             (if (and (not (null? scs)) (null? (cdr scs)))
               (car scs)
               (apply or/sc scs)))]
          [t (t->sc t)])]
       [(Intersection: ts raw-prop)
        (define-values (impersonators chaperones others)
          (for/fold ([imps null]
                     [chaps null]
                     [flats null])
                    ([elem (in-list ts)])
            (define c (t->sc elem))
            (match (get-max-contract-kind c)
              [(== flat-sym) (values imps chaps (cons c flats))]
              [(== chaperone-sym) (values imps (cons c chaps) flats)]
              [(== impersonator-sym) (values (cons c imps) chaps flats)])))
        (define prop
          (cond
            [(TrueProp? raw-prop) #f]
            [else (define x (genid))
                  (define prop (Intersection-prop (-id-path x) type))
                  (define name (format "~a" `(λ (,(syntax->datum x)) ,prop)))
                  (flat-named-lambda/sc name
                                        (id/sc x)
                                        (prop->sc prop))]))
        (cond
          [(> (+ (length impersonators) (length chaperones)) 1)
           (fail #:reason (~a "Intersection type contract contains"
                              " more than 1 non-flat contract: "
                              type))]
          [(and prop (not (null? impersonators)))
           (fail #:reason (~a "Cannot logically refine an impersonated value: "
                              type))]
          [else
           (apply and/sc (append others
                                 chaperones
                                 (if prop (list prop) '())
                                 impersonators))])]
       [(and t (Fun: arrs))
        #:when (any->bool? arrs)
        ;; Avoid putting (-> any T) contracts on struct predicates (where Boolean <: T)
        ;; Optimization: if the value is typed, we can assume it's not wrapped
        ;;  in a type-unsafe chaperone/impersonator and use the unsafe contract
        (define unsafe-spp/sc (flat/sc #'struct-predicate-procedure?))
        (define safe-spp/sc (flat/sc #'struct-predicate-procedure?/c))
        (define optimized/sc (if (from-typed? typed-side) unsafe-spp/sc safe-spp/sc))
        (define spt-pred-procedure?/sc (flat/sc #'struct-type-property-predicate-procedure?))
        (or/sc optimized/sc spt-pred-procedure?/sc (t->sc/fun t))]
       [(? Fun? t) (t->sc/fun t)]
       [(? DepFun? t) (t->sc/fun t)]
       [(Set: t) (set/sc (t->sc t))]
       [(TreeList: t) (treelist/sc (t->sc t))]
       [(Sequence: (list t))
        #:when (subtype t:-Nat t)
        ;; sequence/c is always a wrapper, so avoid it when we just have a number
        (or/sc (flat/sc #'exact-nonnegative-integer?)
               (sequence/sc (t->sc t)))]
       [(Sequence: ts) (apply sequence/sc (map t->sc ts))]
       [(SequenceTop:)
        (only-untyped sequence?/sc)]
       [(Immutable-HeterogeneousVector: ts)
        (apply immutable-vector/sc (map t->sc ts))]
       [(Immutable-Vector: t)
        (immutable-vectorof/sc (t->sc t))]
       [(Mutable-HeterogeneousVector: ts)
        (apply mutable-vector/sc (map t->sc/both ts))]
       [(Mutable-Vector: t)
        (mutable-vectorof/sc (t->sc/both t))]
       [(Mutable-VectorTop:)
        (only-untyped mutable-vector?/sc)]
       [(Box: t) (box/sc (t->sc/both t))]
       [(Pair: t-car t-cdr)
        ;; look ahead as long as t-cdr is a Pair
        (define-values [t-last rev-sc*]
          (let loop ((t t-cdr)
                     (sc* (list (t->sc t-car))))
            (match t
             [(Pair: t-car t-cdr)
              (loop t-cdr (cons (t->sc t-car) sc*))]
             [_
              (values t sc*)])))
        (if (eq? -Null t-last)
          (apply list/sc (reverse rev-sc*))
          (for/fold ((sc-cdr (t->sc t-last)))
                    ((sc (in-list rev-sc*)))
            (cons/sc sc sc-cdr)))]
       [(Async-Channel: t) (async-channel/sc (t->sc t))]
       [(Promise: t)
        (promise/sc (t->sc t))]
       [(Opaque: p?)
        (flat/sc p?)]
       [(Continuation-Mark-Keyof: t)
        (continuation-mark-key/sc (t->sc t))]
       ;; TODO: this is not quite right for case->
       [(Prompt-Tagof: s (Fun: (list (Arrow: ts _ _ _))))
        (prompt-tag/sc (map t->sc ts) (list (t->sc s)))]
       [(Some: (list n) (? Fun? t-body))
        (t->sc/fun t-body #:maybe-existential n)]
       [(F: v)
        (cond
          [(string-prefix? (symbol->string v) "self-")
           (if (not (from-untyped? typed-side))
               ;; if self is in negative position, we can't generate a contract yet.
               (fail #:reason "contract generation not supported for Self")
               any/sc)]
          [else (triple-lookup
                 (hash-ref recursive-values v
                           (λ () (error 'type->static-contract
                                        "Recursive value lookup failed. ~a ~a" recursive-values v)))
                 typed-side)])]
       [(BoxTop:) (only-untyped box?/sc)]
       [(ChannelTop:) (only-untyped channel?/sc)]
       [(Async-ChannelTop:) (only-untyped async-channel?/sc)]
       [(MPairTop:) (only-untyped mpair?/sc)]
       [(ThreadCellTop:) (only-untyped thread-cell?/sc)]
       [(ThreadCell: _) (fail #:reason "contract generation not supported for this type")]
       [(Prompt-TagTop:) (only-untyped prompt-tag?/sc)]
       [(Continuation-Mark-KeyTop:) (only-untyped continuation-mark-key?/sc)]
       [(ClassTop:) (only-untyped class?/sc)]
       [(UnitTop:) (only-untyped unit?/sc)]
       [(StructTypeTop:) (struct-type/sc null)]
       ;; TODO Figure out how this should work
       ;[(StructTop: s) (struct-top/sc s)]


       [(? Poly?)
        (t->sc/poly type fail typed-side recursive-values t->sc)]
       [(? PolyDots?)
        (t->sc/polydots type fail typed-side recursive-values t->sc)]
       [(? PolyRow?)
        (t->sc/polyrow type fail typed-side recursive-values t->sc)]

       [(Mu: (list n) b)
        (match-define (and n*s (list untyped-n* typed-n* both-n*)) (generate-temporaries (list n n n)))
        (define rv
          (hash-set recursive-values n
                    (triple (recursive-sc-use untyped-n*)
                            (recursive-sc-use typed-n*)
                            (recursive-sc-use both-n*))))
        (case typed-side
          [(both) (recursive-sc
                   (list both-n*)
                   (list (loop b 'both rv))
                   (recursive-sc-use both-n*))]
          [(typed untyped)
           (define (rec b side rv)
             (loop b side rv))
           ;; TODO not fail in cases that don't get used
           (define untyped (rec b 'untyped rv))
           (define typed (rec b 'typed rv))
           (define both (rec b 'both rv))

           (recursive-sc
            n*s
            (list untyped typed both)
            (recursive-sc-use (if (from-typed? typed-side) typed-n* untyped-n*)))]
          [else (raise-argument-error 'Mu-case typed-side?-str typed-side)])]
       ;; Don't directly use the class static contract generated for Name,
       ;; because that will get an #:opaque class contract. This will do the
       ;; wrong thing for object types since it errors too eagerly.
       [(Instance: (? Name? t))
        #:when (Class? (resolve-once t))
        (cond [(lookup-name-sc type typed-side)]
              [else
               (define rv recursive-values)
               (define resolved (make-Instance (resolve-once t)))
               (register-name-sc type
                                 (λ () (loop resolved 'untyped rv))
                                 (λ () (loop resolved 'typed rv))
                                 (λ () (loop resolved 'both rv)))
               (lookup-name-sc type typed-side)])]
       [(Instance: (Class: _ _ fields methods _ _))
        (match-define (list (list field-names field-types) ...) fields)
        (match-define (list (list public-names public-types) ...) methods)
        (object/sc (from-typed? typed-side)
                   (append (map (λ (n sc) (member-spec 'method n sc))
                                public-names (map t->sc/meth public-types))
                           (map (λ (n sc) (member-spec 'field n sc))
                                field-names (map t->sc/both field-types))))]
       [(Class: row-var inits fields publics augments _)
        (match-define (list (list init-names init-types _) ...) inits)
        (match-define (list (list field-names field-types) ...) fields)
        (match-define (list (list public-names public-types) ...) publics)
        (match-define (list (list augment-names augment-types) ...) augments)
        (define-values (pubment-names pubment-types)
          (for/lists (_1 _2) ([name (in-list public-names)]
                              [type (in-list public-types)]
                              #:when (memq name augment-names))
            (values name type)))
        (define-values (override-names override-types)
          (for/lists (_1 _2) ([name (in-list public-names)]
                              [type (in-list public-types)]
                              #:unless (memq name pubment-names))
            (values name type)))
        ;; we need to generate absent clauses for non-opaque class contracts
        ;; that occur inside of a mixin type
        (define absents
          (cond [ ;; row constraints are only mapped when it's a row polymorphic
                 ;; function in *positive* position (with no sealing)
                 (and (F? row-var) (lookup-row-constraints (F-n row-var)))
                 =>
                 (λ (constraints)
                   ;; the constraints with no corresponding type/contract need
                   ;; to be absent
                   (append (remove* field-names (cadr constraints))
                           (remove* public-names (caddr constraints))))]
                [else null]))
        ;; add a seal/unseal if there was a row variable and the
        ;; row polymorphic function type was in negative position
        (define seal/sc
          (and (F? row-var)
               (not (lookup-row-constraints (F-n row-var)))
               (triple-lookup
                (hash-ref recursive-values (F-n row-var)
                          (λ () (error 'type->static-contract
                                       "Recursive value lookup failed. ~a ~a"
                                       recursive-values (F-n row-var))))
                typed-side)))
        (define sc-for-class
          (class/sc ;; only enforce opaqueness if there's no row variable
           ;; and we are importing from untyped
           (and (from-untyped? typed-side) (not row-var))
           (append
            (map (λ (n sc) (member-spec 'override n sc))
                 override-names (map t->sc/meth override-types))
            (map (λ (n sc) (member-spec 'pubment n sc))
                 pubment-names (map t->sc/meth pubment-types))
            (map (λ (n sc) (member-spec 'inner n sc))
                 augment-names (map t->sc/meth augment-types))
            (map (λ (n sc) (member-spec 'init n sc))
                 init-names (map t->sc/neg init-types))
            (map (λ (n sc) (member-spec 'field n sc))
                 field-names (map t->sc/both field-types)))
           absents))
        (if seal/sc
            (and/sc seal/sc sc-for-class)
            sc-for-class)]
       [(Unit: imports exports init-depends results)
        (define (traverse sigs)
          (for/list ([sig (in-list sigs)])
            (define mapping
              (map
               (match-lambda
                 [(cons id type) (cons id (t->sc type))])
               (Signature-mapping sig)))
            (signature-spec (Signature-name sig) (map car mapping) (map cdr mapping))))

        (define imports-specs (traverse imports))
        (define exports-specs (traverse exports))
        (define init-depends-ids (map Signature-name init-depends))
        (match results
          [(? AnyValues?)
           (fail #:reason (~a "cannot generate contract for unit type"
                              " with unknown return values"))]
          [(Values: (list (Result: rngs _ _) ...))
           (unit/sc imports-specs exports-specs init-depends-ids (map t->sc rngs))])]
       [(Struct: nm par (list (fld: flds acc-ids mut?) ...) proc poly? pred? props)
        (cond
          [(hash-ref recursive-values nm #f)]
          [proc (fail #:reason "procedural structs are not supported")]
          [poly?
           (struct->recursive-sc #'n* nm flds
                                           (lambda (ftsc)
                                             (struct/sc nm (ormap values mut?) ftsc)))]
          [else (flat/sc #`(flat-named-contract '#,(syntax-e pred?) (lambda (x) (#,pred? x))))])]
       [(StructType: s)
        (if (from-untyped? typed-side)
            (fail #:reason (~a "cannot import structure types from"
                               "untyped code"))
            (struct-type/sc null))]
       [(Struct-Property: s _) (struct-property/sc (t->sc s))]
       [(Has-Struct-Property: orig-id)
        (has-struct-property->sc orig-id)]
       [(Prefab: (and key (list key-sym rst ...)) (list flds ...))
        (cond
          [(hash-ref recursive-values key #f)]
          [else
           (struct->recursive-sc key-sym key flds
                                           (lambda (ftsc)
                                             (prefab/sc key ftsc)))])]
       [(PrefabTop: key)
        (flat/sc #`(struct-type-make-predicate
                    (prefab-key->struct-type (quote #,(abbreviate-prefab-key key))
                                             #,(prefab-key->field-count key))))]
       [(Syntax: (? Base:Symbol?)) identifier?/sc]
       [(Syntax: t)
        (syntax/sc (t->sc t))]
       [(Param: in out)
        (parameter/sc (t->sc in) (t->sc out))]
       [(Mutable-HashTable: k v)
        (mutable-hash/sc (t->sc k) (t->sc v))]
       [(Mutable-HashTableTop:)
        (only-untyped mutable-hash?/sc)]
       [(Immutable-HashTable: k v)
        (immutable-hash/sc (t->sc k) (t->sc v))]
       [(Weak-HashTable: k v)
        (weak-hash/sc (t->sc k) (t->sc v))]
       [(Weak-HashTableTop:)
        (only-untyped weak-hash?/sc)]
       [(Channel: t)
        (channel/sc (t->sc t))]
       [(Evt: t)
        (evt/sc (t->sc t))]
       [(Rest: (list rst-t)) (listof/sc (t->sc rst-t))]
       [(? Rest? rst) (t->sc (Rest->Type rst))]
       [(? Prop? rep) (prop->sc rep)]
       [_
        (fail #:reason "contract generation not supported for this type")]))))

(define (type->static-contract/shallow orig-type #:typed-side [typed-side? #t])
  (let t->sc ([type orig-type]
              [bound-all-vars '()])
    (define (prop->sc p)
      ;;bg copied from above, but uses different t->sc
      (match p
        [(TypeProp: o t)
         (define sc (t->sc t bound-all-vars))
         (unless (equal? flat-sym (get-max-contract-kind sc))
           (raise-user-error 'type->static-contract/shallow
                             "proposition contract generation not supported for non-flat types"))
         (is-flat-type/sc (obj->sc o) sc)]
        [(NotTypeProp: o t)
         (define sc (t->sc t bound-all-vars))
         (unless (equal? flat-sym (get-max-contract-kind sc))
           (raise-user-error 'type->static-contract/shallow
                             "proposition contract generation not supported for non-flat types"))
         (not-flat-type/sc (obj->sc o) sc)]
        [(LeqProp: (app obj->sc lhs) (app obj->sc rhs))
         (leq/sc lhs rhs)]
        [(AndProp: ps)
         (and-prop/sc (map prop->sc ps))]
        [(OrProp: ps)
         (or-prop/sc (map prop->sc ps))]))
    (match type
     ;; Implicit recursive aliases
     [(Name: _name-id _args #f)
      (cond [(lookup-name-sc type 'both) ]
            [else
             (define resolved-name (resolve-once type))
             (register-name-sc type
                               (λ () (t->sc resolved-name bound-all-vars))
                               (λ () (t->sc resolved-name bound-all-vars))
                               (λ () (t->sc resolved-name bound-all-vars)))
             (lookup-name-sc type 'both)])]
     ;; Ordinary type applications or struct type names, just resolve
     [(or (App: _ _)
          (Name/struct:))
      (t->sc (resolve-once type) bound-all-vars)]
     [(Univ:) any/sc]
     [(Bottom:) (shallow-or/sc)]
     ;; This comes before Base-ctc to use the Value-style logic
     ;; for the singleton base types (e.g. -Null, 1, etc)
     [(Val-able: v)
      (cond
       [(eof-object? v)
        (flat/sc #'eof-object?)]
       [(void? v)
        (flat/sc #'void?)]
       [(or (symbol? v) (boolean? v) (keyword? v) (null? v) (eq? unsafe-undefined v))
        (flat/sc #`(lambda (x) (eq? x '#,v)))]
       [(or (number? v) (regexp? v) (byte-regexp? v) (string? v) (bytes? v) (char? v))
        (flat/sc #`(lambda (x) (equal? x '#,v)))]
       [else
        (raise-arguments-error 'type->static-contract/shallow "unexpected Val-able value" "value" v "original type" type)])]
     [(Base-name/contract: sym ctc) (flat/sc ctc)]
     [(Distinction: _ _ t) ; from define-new-subtype
      (t->sc t bound-all-vars)]
     [(Refinement: par p?)
      (shallow-and/sc (t->sc par bound-all-vars) (flat/sc p?))]
     [(BaseUnion: bbits nbits)
      (define numeric (make-BaseUnion #b0 nbits))
      (define other-scs
        (for/list ((base-t (in-list (bbits->base-types bbits))))
          (t->sc base-t bound-all-vars)))
      (define numeric-sc (numeric-type->static-contract numeric))
      (if numeric-sc
          (apply shallow-or/sc numeric-sc other-scs)
          (apply shallow-or/sc (append other-scs
                                         (for/list ((base-t (in-list (nbits->base-types nbits))))
                                            (t->sc base-t bound-all-vars)))))]
     [(? Union? t)
      (match (normalize-type t)
        [(Union-all-flat: elems)
         (let* ([sc* (for/list ((e (in-list elems)))
                       (t->sc e bound-all-vars))]
                [sc* (remove-duplicates sc*)]
                [sc* (remove-overlap sc*
                       (list
                         (cons vector?/sc (list mutable-vector?/sc immutable-vector?/sc))
                         (cons hash?/sc (list mutable-hash?/sc weak-hash?/sc immutable-hash?/sc))))])
           (apply shallow-or/sc sc*))]
        [t (t->sc t bound-all-vars)])]
     [(Intersection: ts raw-prop)
      (define scs
        (for/list ((t (in-list ts)))
          (t->sc t bound-all-vars)))
      (define prop/sc
        (cond
          [(TrueProp? raw-prop) #f]
          [else (define x (genid))
                (define prop (Intersection-prop (-id-path x) type))
                (define name (format "~a" `(λ (,(syntax->datum x)) ,prop)))
                (flat-named-lambda/sc name
                                      (id/sc x)
                                      (prop->sc prop))]))
      (apply shallow-and/sc (append scs (if prop/sc (list prop/sc) '())))]
     [(Fun: arrows)
      (if (null? arrows)
        procedure?/sc
        (apply shallow-and/sc
               (for/list ((arr (in-list arrows)))
                 (arrow->sc/shallow arr typed-side?))))]
     [(DepFun: raw-dom _ rng)
      (define num-mand-args (length raw-dom))
      (if (and (not typed-side?) (arrow-rng-has-prop? rng))
        none/sc
        (make-procedure-arity-flat/sc num-mand-args '() '()))]
     [(Set: _) set?/sc]
     [(TreeList: _) treelist?/sc] 
     [(or (Sequence: _)
          (SequenceTop:)
          (SequenceDots: _ _ _))
      sequence?/sc]
     [(Immutable-HeterogeneousVector: ts)
      (immutable-vector-length/sc (length ts))]
     [(Immutable-Vector: _)
      immutable-vector?/sc]
     [(Mutable-HeterogeneousVector: ts)
      (mutable-vector-length/sc (length ts))]
     [(or (Mutable-Vector: _)
          (Mutable-VectorTop:))
      mutable-vector?/sc]
     [(or (Box: _)
          (BoxTop:))
      box?/sc]
     [(or (Weak-Box: _)
          (Weak-BoxTop:))
      weak-box?/sc]
     [(or (Listof: _)
          (ListDots: _ _))
      list?/sc]
     [(Pair: _ t-cdr)
      ;; look ahead, try making list/sc
      (let cdr-loop ((t t-cdr)
                     (num-elems 1))
        (match t
         [(Pair: _ t-cdr)
          (cdr-loop t-cdr (+ num-elems 1))]
         [(== -Null)
          (list-length/sc num-elems)]
         [_
          cons?/sc]))]
     [(or (Async-Channel: _)
          (Async-ChannelTop:))
      async-channel?/sc]
     [(Promise: _)
      promise?/sc]
     [(Opaque: p?)
      (flat/sc p?)]
     [(or (Continuation-Mark-Keyof: _)
          (Continuation-Mark-KeyTop:))
      continuation-mark-key?/sc]
     [(or (Prompt-Tagof: _ _)
          (Prompt-TagTop:))
      prompt-tag?/sc]
     [(F: v)
      (if (member v bound-all-vars)
        none/sc
        any/sc)]
     [(or (MPair: _ _)
          (MPairTop:))
      mpair?/sc]
     [(or (ThreadCell: _)
          (ThreadCellTop:))
      thread-cell?/sc]
     [(ClassTop:) class?/sc]
     [(UnitTop:) unit?/sc]
     [(or (Poly: vs b)
          (PolyDots: (list vs ... _) b)
          (PolyRow: vs b _))
      (t->sc b (append bound-all-vars vs))]
     [(Mu: n b)
      (t->sc b bound-all-vars)]
     [(Instance: (? Name? t))
      #:when (Class? (resolve-once t))
      (cond [(lookup-name-sc type 'both)]
            [else
             (define resolved (make-Instance (resolve-once t)))
             (register-name-sc type
                               (λ () (t->sc resolved bound-all-vars))
                               (λ () (t->sc resolved bound-all-vars))
                               (λ () (t->sc resolved bound-all-vars)))
             (lookup-name-sc type 'both)])]
     [(Instance: (Class: _ _ fields methods _ _))
      (make-object-shape/sc (map car fields) (map car methods))]
     [(Class: row-var inits fields publics augments _)
      (make-class-shape/sc (map car inits) (map car fields) (map car publics) (map car augments))]
     [(Unit: imports exports init-depends results)
      unit?/sc]
     [(or (Struct: _ _ _ _ _ pred? _)
          (StructTop: (Struct: _ _ _ _ _ pred? _)))
      (flat/sc #`(lambda (x) (#,pred? x)))]
     [(StructTypeTop:)
      struct-type?/sc]
     [(StructType: s)
      (t->sc s bound-all-vars)]
     [(Struct-Property: s _)
      struct-type-property?/sc]
     [(Has-Struct-Property: orig-id)
      (has-struct-property->sc orig-id)]
     [(or (Prefab: key _)
          (PrefabTop: key))
      (flat/sc #`(struct-type-make-predicate
                  (prefab-key->struct-type (quote #,(abbreviate-prefab-key key))
                                           #,(prefab-key->field-count key))))]
     [(Syntax: (? Base:Symbol?))
      identifier?/sc]
     [(Syntax: t)
      syntax?/sc]
     [(Param: in out)
      parameter?/sc]
     [(or (Mutable-HashTable: _ _)
          (Mutable-HashTableTop:))
      mutable-hash?/sc]
     [(Immutable-HashTable: _ _)
      immutable-hash?/sc]
     [(or (Weak-HashTable: _ _)
          (Weak-HashTableTop:))
      weak-hash?/sc]
     [(or (Channel: _)
          (ChannelTop:))
      channel?/sc]
     [(Evt: t)
      evt?/sc]
     [(? Prop? rep) (prop->sc rep)]
     [(Ephemeron: _)
      ephemeron?/sc]
     [(Future: _)
      future?/sc]
     [_
      (raise-arguments-error 'type->static-contract/shallow "contract generation not supported for this type" "type" type "original" orig-type)])))

(define (remove-overlap sc* pattern*)
  (for/fold ((acc sc*))
            ((kv* (in-list pattern*)))
    (define replacement (car kv*))
    (define tgt* (cdr kv*))
    (define-values [success? acc+] (remove** tgt* acc))
    (if success?
      (cons replacement acc+)
      acc)))

(define (remove** target* sc*)
  (for/fold ((success? #t)
             (sc* sc*))
            ((t (in-list target*)))
    (values (and success?
                 (member t sc*))
            (filter (lambda (x) (not (equal? x t))) sc*))))

(define (obj->sc o)
  (match o
    [(Path: pes (? identifier? x))
     (for/fold ([obj (id/sc x)])
               ([pe (in-list (reverse pes))])
       (match pe
         [(CarPE:) (acc-obj/sc #'car obj)]
         [(CdrPE:) (acc-obj/sc #'cdr obj)]
         [(VecLenPE:) (acc-obj/sc #'vector-length obj)]))]
    [(LExp: const terms)
     (linear-exp/sc const
                    (for/hash ([(obj coeff) (in-terms terms)])
                      (values (obj->sc obj) coeff)))]
    [else
      (raise-argument-error 'obj->sc "Object?" o)]))

(define (partition-kws kws)
  (partition (match-lambda [(Keyword: _ _ mand?) mand?]) kws))

(define arrow->sc/shallow
  (let ((conv (match-lambda [(Keyword: kw _ _) kw])))
    (lambda (orig-ty typed-side?)
      (match orig-ty
        [(Arrow: _ _ _ rng)
         #:when (and (not typed-side?) (arrow-rng-has-prop? rng))
         none/sc]
        [(Arrow: _ (RestDots: _ _) _ _)
         procedure?/sc]
        [(Arrow: dom _ kws _)
         (define num-mand-args (length dom))
         (define-values [mand-kws opt-kws]
           (let-values ([(mand-kws opt-kws) (partition-kws kws)])
             (values (map conv mand-kws) (map conv opt-kws))))
         (make-procedure-arity-flat/sc num-mand-args mand-kws opt-kws)]))))

(define (arrow-rng-has-prop? rng)
  (match rng
    [(Values: (list (Result: _
                             (PropSet: (TrueProp:)
                                       (TrueProp:))
                             (Empty:)) ...))
     #f]
    ;; Functions that don't return
    [(Values: (list (Result: (== -Bottom) _ _) ...))
     #f]
    ;; functions with props or objects
    [(Values: (list (Result: rngs _ _) ...))
     #true]
    [(? ValuesDots?)
     #f]
    [(? AnyValues?)
     #f]))

(define (make-procedure-arity-flat/sc num-mand mand-kws opt-kws)
  (flat/sc
    #`(λ (f)
        (and (procedure? f)
             (procedure-arity-includes? f '#,num-mand '#,(not (null? mand-kws)))
             #,@(if (and (null? mand-kws) (null? opt-kws))
                  #'()
                  #`((procedure-arity-includes-keywords? f '#,mand-kws '#,opt-kws)))))))

(define (t->sc/function f fail typed-side recursive-values loop method? #:maybe-existential [opt-exi #f])
  (define (t->sc t #:recursive-values (recursive-values recursive-values))
    (loop t typed-side recursive-values))
  (define (t->sc/neg t #:recursive-values (recursive-values recursive-values))
    (loop t (flip-side typed-side) recursive-values))

  (define (arr-params->exist/sc exi dom rst kw rng prop+type)
    (define (occur? t)
      (if (or (not t) (empty? t)) #f
          (set-member? (free-vars-names (free-vars* t)) exi)))

    (match* (rng prop+type)
      [((Fun: (list (Arrow: (list-rest (F: n1) a ... _) rst_i kw_i _))) (F: n1))
       #:when (and (not (ormap occur? (list rst kw rst_i kw_i)))
                   (eq? n1 exi))
       (void)]
      [(_ _) (fail #:reason
                   "contract generation only supports Some Type in this form: (Some (X) (-> ty1 ... (-> X ty ... ty2) : X)) or (-> ty1 ... (Some (X) (-> X ty ... ty2) : X))")])

    (define/with-syntax name exi)
    (define lhs (t->sc/neg dom))
    (define eq-name (flat/sc #'(eq/c name)))
    (define rhs (t->sc rng
                       #:recursive-values (hash-set recursive-values exi
                                                    (same eq-name))))
    (exist/sc (list #'name) lhs rhs))


  ;; handle-arrow-range : Arr (-> Static-Contact) -> Static-Contract
  ;; Match the range of an arr and determine if a contract can be generated
  ;; and call the given thunk or raise an error
  (define (handle-arrow-range arrow proceed)
    (match-define (or (Arrow: _ _ _ rng) (DepFun: _ _ rng)) arrow)
    (handle-range rng proceed))
  (define (handle-range rng proceed)
    (match rng
      [(Values: (list (Result: _
                               (PropSet: (TrueProp:)
                                         (TrueProp:))
                               (Empty:)) ...))
       (proceed)]
      ;; Functions that don't return
      [(Values: (list (Result: (== -Bottom) _ _) ...))
       (proceed)]
      ;; functions with props or objects
      [(Values: (list (Result: rngs _ _) ...))
       (if (from-untyped? typed-side)
           (fail #:reason (~a "cannot generate contract for function type"
                              " with props or objects."))
           (proceed))]
      [(? ValuesDots?)
       (fail #:reason (~a "cannot generate contract for function type"
                          " with dotted return values"))]
      [(? AnyValues?)
       (fail #:reason (~a "cannot generate contract for function type"
                          " with unknown return values"))]))
  (match f
    [(Fun: arrows)
     ;; Try to generate a single `->*' contract if possible.
     ;; This allows contracts to be generated for functions with both optional and keyword args.
     ;; (and don't otherwise require full `case->')

     (define conv (match-lambda [(Keyword: kw kty _) (list kw (t->sc/neg kty))]))
     (define (partition-kws kws) (partition (match-lambda [(Keyword: _ _ mand?) mand?]) kws))
     (define (process-dom dom*)  (if method? (cons any/sc dom*) dom*))
     (cond
       ;; To generate a single `->*':
       ;; - keywords and range must be the same for all arrows
       ;; - only the last arrow may have a rest argument
       ;; - positional argument count increases by one at each step
       ;; Note: optional arguments can only increase by 1 each time, to avoid problems with
       ;;  functions that take, e.g., either 2 or 6 arguments. These functions shouldn't match,
       ;;  since this code would generate contracts that accept any number of arguments between
       ;;  2 and 6, which is wrong.
       ;; TODO sufficient condition, but may not be necessary
       [(has-optional-args? arrows)
        (define first-arrow (first arrows))
        (define last-arrow (last arrows))
        (define (convert-arrow)
          (match-define (Arrow: first-dom _ kws
                                (Values: (list (Result: rngs _ _) ...)))
            first-arrow)
          (define rst (Arrow-rst last-arrow))

          ;; kws and rng same for all arrs
          (define last-dom (Arrow-dom last-arrow))
          (define mand-args (map t->sc/neg first-dom))
          (define opt-args (map t->sc/neg (drop last-dom (length first-dom))))
          (define-values (mand-kws opt-kws)
            (let*-values ([(mand-kws opt-kws) (partition-kws kws)])
              (values (map conv mand-kws)
                      (map conv opt-kws))))
          (define range (map t->sc rngs))
          (define rest (and rst (t->sc/neg rst)))
          (function/sc (from-typed? typed-side) (process-dom mand-args) opt-args mand-kws opt-kws rest range))
        (handle-arrow-range first-arrow convert-arrow)]
       [else
        (define/match (convert-single-arrow arr)
          [((Arrow: (list dom) rst kws (Values: (list (Result: rng (PropSet: (TypeProp: _ prop+type) _) _ 0)))))
           #:when opt-exi
           (let-values ([(mand-kws opt-kws) (partition-kws kws)])
             (arr-params->exist/sc opt-exi dom rst kws rng prop+type))]

          [((Arrow: (list dom) rst kws (Values: (list (ExistentialResult: rng (PropSet: (TypeProp: _ prop+type) _) _ vars)))))
           (let*-values ([(mand-kws opt-kws) (partition-kws kws)])
             (arr-params->exist/sc (F-n (car vars)) dom rst kws rng prop+type))]

          [((Arrow: dom rst kws (Values: (list (Result: rngs _ _ n-exiss) ...))))
           (define-values (mand-kws opt-kws) (partition-kws kws))
           (function/sc
                 (from-typed? typed-side)
                 (process-dom (map t->sc/neg dom))
                 null
                 (map conv mand-kws)
                 (map conv opt-kws)
                 (match rst
                   [(? Rest?) (t->sc/neg rst)]
                   [(RestDots: dty dbound)
                    (listof/sc
                     (t->sc/neg dty
                                #:recursive-values
                                (hash-set recursive-values dbound (same any/sc))))]
                   [_ #f])
                 (map t->sc rngs))])

        (define/match (convert-one-arrow-in-many arr)
          [((Arrow: dom rst kws (Values: (list (Result: rngs _ _ n-exis) ...))))
           (let-values ([(mand-kws opt-kws) (partition-kws kws)])
             ;; Garr, I hate case->!
             (when (and (not (empty? kws)))
               (fail #:reason (~a "cannot generate contract for case function type"
                                  " with optional keyword arguments")))
             (when (ormap positive?
                          n-exis)
               (fail #:reason (~a "cannot generate contract for case function type with existentials")))

             (arr/sc (process-dom (map t->sc/neg dom))
                     (and rst (t->sc/neg rst))
                     (map t->sc rngs)))])

        (define arities
          (for/list ([t (in-list arrows)]) (length (Arrow-dom t))))

        (define maybe-dup (check-duplicates arities))
        (when maybe-dup
          (fail #:reason (~a "function type has two cases of arity " maybe-dup)))

        (if (= (length arrows) 1)
            (handle-arrow-range (first arrows)
                                (lambda ()
                                  (convert-single-arrow (first arrows))))
            (case->/sc (for/list ([arr (in-list arrows)])
                         (handle-arrow-range arr (lambda () (convert-one-arrow-in-many arr))))))])]
    [(DepFun/ids: ids dom pre rng)
     (define (continue)
       (match-define (Values: (list (Result: rngs _ _) ...)) rng)
       (define (dom-id? id)
         (member id ids free-identifier=?))
       (define-values (dom* dom-deps)
         (for/lists (_1 _2) ([d (in-list dom)]) (values (t->sc/neg d) (filter dom-id? (free-ids d)))))
       (define pre*
         (if (TrueProp? pre)
             #f
             (t->sc/neg pre)))
       (define pre-deps (filter dom-id? (free-ids pre)))
       (define rng* (map t->sc rngs))
       (define rng-deps
         (filter dom-id? (remove-duplicates (apply append (map free-ids rngs)) free-identifier=?)))
       (->i/sc (from-typed? typed-side) ids dom* dom-deps pre* pre-deps rng* rng-deps))
     (handle-range rng continue)]))

;; Generate a contract for a object/class method clause
;; Precondition: type is a valid method type
(define (t->sc/method type fail typed-side recursive-values loop)
  ;; helper for mutually recursive calls in Poly cases
  (define (rec body #:recursive-values rv)
    (t->sc/method body fail typed-side rv loop))
  (match type
    [(? Poly?)
     (t->sc/poly type fail typed-side recursive-values rec)]
    [(? PolyDots?)
     (t->sc/polydots type fail typed-side recursive-values rec)]
    [(? PolyRow?)
     (t->sc/polyrow type fail typed-side recursive-values rec)]
    [(? Fun?)
     (t->sc/function type fail typed-side recursive-values loop #t)]
    [_ (fail #:reason "invalid method type")]))

(define (is-a-function-type? initial)
  (let loop ([ty initial])
    (match (resolve ty)
      [(? Fun?) #t]
      [(? DepFun?) #t]
      [(Union: _ elems) (andmap loop elems)]
      [(Intersection: elems _) (ormap loop elems)]
      [(Poly: _ body) (loop body)]
      [(PolyDots: _ body) (loop body)]
      [_ #f])))

;; Generate a contract for a polymorphic function type
(define (t->sc/poly type fail typed-side recursive-values t->sc)
  (match-define (Poly: vs b) type)
  (if (not (from-untyped? typed-side))
      ;; in positive position, no checking needed for the variables
      (let ((recursive-values (for/fold ([rv recursive-values]) ([v vs])
                                (hash-set rv v (same any/sc)))))
        (t->sc b #:recursive-values recursive-values))
      ;; in negative position, use parametric contracts.
      (match-let ([(Poly-names: vs-nm b) type])
        (unless (is-a-function-type? b)
          (fail #:reason "cannot generate contract for non-function polymorphic type"))
        (let ((temporaries (generate-temporaries vs-nm)))
          (define rv (for/fold ((rv recursive-values)) ((temp temporaries)
                                                        (v-nm vs-nm))
                       (hash-set rv v-nm (same (parametric-var/sc temp)))))
          (parametric->/sc temporaries
            (t->sc b #:recursive-values rv))))))

;; Generate a contract for a variable-arity polymorphic function type
(define (t->sc/polydots type fail typed-side recursive-values t->sc)
  (match-define (PolyDots: (list vs ... dotted-v) b) type)
  (if (not (from-untyped? typed-side))
      ;; in positive position, no checking needed for the variables
      (let ((recursive-values (for/fold ([rv recursive-values]) ([v vs])
                                (hash-set rv v (same any/sc)))))
        (t->sc b #:recursive-values recursive-values))
      ;; in negative position, cannot generate for polydots yet
      (fail #:reason "cannot generate contract for variable arity polymorphic type")))

;; Generate a contract for a row-polymorphic function type
(define (t->sc/polyrow type fail typed-side recursive-values t->sc)
  (match-define (PolyRow: vs body constraints) type)
  (if (not (from-untyped? typed-side))
      (let ((recursive-values (for/fold ([rv recursive-values]) ([v vs])
                                (hash-set rv v (same any/sc)))))
        (extend-row-constraints vs (list constraints)
          (t->sc body #:recursive-values recursive-values)))
      (match-let ([(PolyRow-names: vs-nm b constraints) type])
        (unless (is-a-function-type? b)
          (fail #:reason "cannot generate contract for non-function polymorphic type"))
        (let ([temporaries (generate-temporaries vs-nm)])
          (define rv (for/fold ([rv recursive-values])
                               ([temp temporaries]
                                [v-nm vs-nm])
                       (hash-set rv v-nm (same (sealing-var/sc temp)))))
          ;; Only the first three sets of constraints seem to be needed
          ;; since augment clauses don't make sense without a corresponding
          ;; public method too. This invariant has to be enforced though.
          (sealing->/sc temporaries (take constraints 3)
            (t->sc b #:recursive-values rv))))))

(define (has-struct-property->sc orig-id)
  ;; we can't call syntax-local-value/immediate in has-struct-property case in parse-type
  (define-values (a prop-name) (syntax-local-value/immediate orig-id (λ () (values #t orig-id))))
  (match-define (Struct-Property: _ pred?) (lookup-id-type/lexical prop-name))
  ;; if original-name is only set when the type is added via require/typed

  ;; the original-name of `prop-name` is its original referece in the unexpanded program.
  (define real-prop-var (or (syntax-property prop-name 'original-name) prop-name))

  ;; a property is wrapped so we need its original reference
  (define real-pred-var (or (syntax-property pred? 'original-name) (syntax-e pred?)))

  ;; the `pred?` could be provided to a property through require/typed,
  ;; so we need to check if it is produced by the property
  (flat/sc #`(flat-named-contract '#,real-pred-var
                                  (lambda (x)
                                    (if (not (struct-type-property-predicate-procedure? #,pred? #,real-prop-var))
                                        (raise-arguments-error 'struct-property
                                                               "predicate does not match property"
                                                               "predicate"
                                                               #,pred?
                                                               "property"
                                                               #,real-prop-var)
                                        (#,pred? x))))))

;; Predicate that checks for an App type with a recursive
;; Name type in application position
(define (has-name-app? type)
  (let/ec escape
    (let loop ([rep type])
      (match rep
        [(App: (Name: _ _ #f) _) (escape #t)]
        [_ (Rep-for-each rep loop)]))
    #f))

;; True if the arities `arrs` are what we'd expect from a struct predicate
(define (any->bool? arrs)
  (match arrs
    [(list (Arrow: (list (Univ:))
                   #f '()
                   (Values: (list (Result: t _ _)))))
     (t:subtype -Boolean t)]
    [_ #f]))

(define (merge-hash-types tys t->sc top-sc)
  (merge-types->scs
    tys
    #:t->elems hash-type->elems
    #:elems->sc (lambda (k v) (hash/sc (t->sc k) (t->sc v)))
    #:t->sc t->sc
    #:top (match-lambda
           [(list-no-order (Immutable-HashTable: (Univ:) (Univ:))
                           (Mutable-HashTableTop:)
                           (Weak-HashTableTop:))
            top-sc]
           [_ #f])))

(define (merge-vector-types tys t->sc top-sc)
  (merge-types->scs
    tys
    #:t->elems vector-type->elems
    #:elems->sc (lambda (t) (vectorof/sc (t->sc t)))
    #:t->sc t->sc
    #:top (match-lambda
           [(list-no-order (Immutable-Vector: (Univ:))
                           (Mutable-VectorTop:))
            top-sc]
           [_ #f])))

(define (merge-heterogeneous-vector-types tys t->sc)
  (merge-types->scs
    tys
    #:t->elems heterogeneous-vector-type->elems
    #:elems->sc (lambda ts (apply vector/sc (map t->sc ts)))
    #:t->sc t->sc))

;; Convert a list of types to a (shorter) list of static contracts
;; by (1) checking whether the list represents a top type,
;; and (2) grouping types with similar elements
(define (merge-types->scs tys0
                          #:t->elems t->elems
                          #:elems->sc elems->sc
                          #:t->sc t->sc
                          #:top [top #f])
  (define top-sc (and top (top tys0)))
  (if top-sc
    (list top-sc)
    (for/list ((tys (in-list (group-types-by-elems t->elems tys0))))
      (if (null? (cdr tys))
        (t->sc (car tys))
        (apply elems->sc (t->elems (car tys)))))))

(define (hash-type? ty)
  (match ty
   [(or (Mutable-HashTable: _ _)
        (Mutable-HashTableTop:)
        (Immutable-HashTable: _ _)
        (Weak-HashTable: _ _)
        (Weak-HashTableTop:))
    #true]
   [_ #false]))

(define (vector-type? ty)
  (match ty
   [(or (Immutable-Vector: _)
        (Mutable-Vector: _)
        (Mutable-VectorTop:))
    #true]
   [_ #false]))

(define (heterogeneous-vector-type? ty)
  (match ty
   [(or (Immutable-HeterogeneousVector: _)
        (Mutable-HeterogeneousVector: _))
    #true]
   [_ #false]))

(define (group-types-by-elems t->elems ts)
  (group-by t->elems ts non-#f-and-equal?))

(define (non-#f-and-equal? a b)
  (and a b (equal? a b)))

(define (hash-type->elems ty)
  (match ty
   [(or (Mutable-HashTable: k v)
        (Immutable-HashTable: k v)
        (Weak-HashTable: k v))
    (list k v)]
   [_ #false]))

(define (vector-type->elems ty)
  (match ty
   [(or (Immutable-Vector: t)
        (Mutable-Vector: t))
    (list t)]
   [_ #false]))

(define (heterogeneous-vector-type->elems ty)
  (match ty
   [(or (Immutable-HeterogeneousVector: ts)
        (Mutable-HeterogeneousVector: ts))
    ts]
   [_ #false]))

(module predicates racket/base
  (require racket/extflonum)
  (provide nonnegative? nonpositive?
           extflonum? extflzero? extflnonnegative? extflnonpositive?)
  (define (nonnegative? x)
    (>= x 0))
  (define (nonpositive? x)
    (<= x 0))
  (define (extflzero? x)
    (extfl= x 0.0t0))
  (define (extflnonnegative? x)
    (extfl>= x 0.0t0))
  (define (extflnonpositive? x)
    (extfl<= x 0.0t0)))

(module numeric-contracts racket/base
  (require
    "../utils/utils.rkt"
    "../static-contracts/combinators.rkt"
    (for-template
      racket/base
      racket/contract
      (submod ".." predicates)
      (prefix-in t: (types numeric-predicates))))
  (provide (all-defined-out))

  (define-syntax-rule (numeric/sc name body) (flat/sc #'body))

  (define positive-byte/sc (numeric/sc Positive-Byte (and/c byte? positive?)))
  (define byte/sc (numeric/sc Byte byte?))
  (define positive-index/sc (numeric/sc Positive-Index (and/c t:index? positive?)))
  (define index/sc (numeric/sc Index t:index?))
  (define positive-fixnum/sc (numeric/sc Positive-Fixnum (and/c fixnum? positive?)))
  (define nonnegative-fixnum/sc (numeric/sc Nonnegative-Fixnum (and/c fixnum? nonnegative?)))
  (define nonpositive-fixnum/sc (numeric/sc Nonpositive-Fixnum (and/c fixnum? nonpositive?)))
  (define fixnum/sc (numeric/sc Fixnum fixnum?))
  (define positive-integer/sc (numeric/sc Positive-Integer (and/c exact-integer? positive?)))
  (define natural/sc (numeric/sc Natural exact-nonnegative-integer?))
  (define negative-integer/sc (numeric/sc Negative-Integer (and/c exact-integer? negative?)))
  (define nonpositive-integer/sc (numeric/sc Nonpositive-Integer (and/c exact-integer? nonpositive?)))
  (define integer/sc (numeric/sc Integer exact-integer?))
  (define positive-rational/sc (numeric/sc Positive-Rational (and/c t:exact-rational? positive?)))
  (define nonnegative-rational/sc (numeric/sc Nonnegative-Rational (and/c t:exact-rational? nonnegative?)))
  (define negative-rational/sc (numeric/sc Negative-Rational (and/c t:exact-rational? negative?)))
  (define nonpositive-rational/sc (numeric/sc Nonpositive-Rational (and/c t:exact-rational? nonpositive?)))
  (define rational/sc (numeric/sc Rational t:exact-rational?))
  (define flonum-zero/sc (numeric/sc Float-Zero (and/c flonum? zero?)))
  (define nonnegative-flonum/sc (numeric/sc Nonnegative-Float (and/c flonum? nonnegative?)))
  (define nonpositive-flonum/sc (numeric/sc Nonpositive-Float (and/c flonum? nonpositive?)))
  (define flonum/sc (numeric/sc Float flonum?))
  (define single-flonum-zero/sc (numeric/sc Single-Flonum-Zero (and/c single-flonum? zero?)))
  (define inexact-real-zero/sc (numeric/sc Inexact-Real-Zero (and/c inexact-real? zero?)))
  (define positive-inexact-real/sc (numeric/sc Positive-Inexact-Real (and/c inexact-real? positive?)))
  (define nonnegative-single-flonum/sc (numeric/sc Nonnegative-Single-Flonum (and/c single-flonum? nonnegative?)))
  (define nonnegative-inexact-real/sc (numeric/sc Nonnegative-Inexact-Real (and/c inexact-real? nonnegative?)))
  (define negative-inexact-real/sc (numeric/sc Negative-Inexact-Real (and/c inexact-real? negative?)))
  (define nonpositive-single-flonum/sc (numeric/sc Nonpositive-Single-Flonum (and/c single-flonum? nonpositive?)))
  (define nonpositive-inexact-real/sc (numeric/sc Nonpositive-Inexact-Real (and/c inexact-real? nonpositive?)))
  (define single-flonum/sc (numeric/sc Single-Flonum single-flonum?))
  (define inexact-real/sc (numeric/sc Inexact-Real inexact-real?))
  (define real-zero/sc (numeric/sc Real-Zero (and/c real? zero?)))
  (define positive-real/sc (numeric/sc Positive-Real (and/c real? positive?)))
  (define nonnegative-real/sc (numeric/sc Nonnegative-Real (and/c real? nonnegative?)))
  (define negative-real/sc (numeric/sc Negative-Real (and/c real? negative?)))
  (define nonpositive-real/sc (numeric/sc Nonpositive-Real (and/c real? nonpositive?)))
  (define real/sc (numeric/sc Real real?))
  (define exact-number/sc (numeric/sc Exact-Number (and/c number? exact?)))
  (define inexact-complex/sc
    (numeric/sc Inexact-Complex
                (and/c
                  number?
                  (lambda (x)
                    (and (inexact-real? (imag-part x))
                         (inexact-real? (real-part x)))))))
  (define number/sc (numeric/sc Number number?))

  (define extflonum-zero/sc (numeric/sc ExtFlonum-Zero (and/c extflonum? extflzero?)))
  (define nonnegative-extflonum/sc (numeric/sc Nonnegative-ExtFlonum (and/c extflonum? extflnonnegative?)))
  (define nonpositive-extflonum/sc (numeric/sc Nonpositive-ExtFlonum (and/c extflonum? extflnonpositive?)))
  (define extflonum/sc (numeric/sc ExtFlonum extflonum?))

  )
(require 'numeric-contracts)

(define (numeric-type->static-contract type)
  (match type
    ;; numeric special cases
    ;; since often-used types like Integer are big unions, this would
    ;; generate large contracts.
    [(== t:-PosByte) positive-byte/sc]
    [(== t:-Byte) byte/sc]
    [(== t:-PosIndex) positive-index/sc]
    [(== t:-Index) index/sc]
    [(== t:-PosFixnum) positive-fixnum/sc]
    [(== t:-NonNegFixnum) nonnegative-fixnum/sc]
    ;; -NegFixnum is a base type
    [(== t:-NonPosFixnum) nonpositive-fixnum/sc]
    [(== t:-Fixnum) fixnum/sc]
    [(== t:-PosInt) positive-integer/sc]
    [(== t:-Nat) natural/sc]
    [(== t:-NegInt) negative-integer/sc]
    [(== t:-NonPosInt) nonpositive-integer/sc]
    [(== t:-Int) integer/sc]
    [(== t:-PosRat) positive-rational/sc]
    [(== t:-NonNegRat) nonnegative-rational/sc]
    [(== t:-NegRat) negative-rational/sc]
    [(== t:-NonPosRat) nonpositive-rational/sc]
    [(== t:-Rat) rational/sc]
    [(== t:-FlonumZero) flonum-zero/sc]
    [(== t:-NonNegFlonum) nonnegative-flonum/sc]
    [(== t:-NonPosFlonum) nonpositive-flonum/sc]
    [(== t:-Flonum) flonum/sc]
    [(== t:-SingleFlonumZero) single-flonum-zero/sc]
    [(== t:-InexactRealZero) inexact-real-zero/sc]
    [(== t:-PosInexactReal) positive-inexact-real/sc]
    [(== t:-NonNegSingleFlonum) nonnegative-single-flonum/sc]
    [(== t:-NonNegInexactReal) nonnegative-inexact-real/sc]
    [(== t:-NegInexactReal) negative-inexact-real/sc]
    [(== t:-NonPosSingleFlonum) nonpositive-single-flonum/sc]
    [(== t:-NonPosInexactReal) nonpositive-inexact-real/sc]
    [(== t:-SingleFlonum) single-flonum/sc]
    [(== t:-InexactReal) inexact-real/sc]
    [(== t:-RealZero) real-zero/sc]
    [(== t:-PosReal) positive-real/sc]
    [(== t:-NonNegReal) nonnegative-real/sc]
    [(== t:-NegReal) negative-real/sc]
    [(== t:-NonPosReal) nonpositive-real/sc]
    [(== t:-Real) real/sc]
    [(== t:-ExactNumber) exact-number/sc]
    [(== t:-InexactComplex) inexact-complex/sc]
    [(== t:-Number) number/sc]
    [(== t:-ExtFlonumZero) extflonum-zero/sc]
    [(== t:-NonNegExtFlonum) nonnegative-extflonum/sc]
    [(== t:-NonPosExtFlonum) nonpositive-extflonum/sc]
    [(== t:-ExtFlonum) extflonum/sc]
    [else #f]))
