#lang racket/unit

;; This module provides a unit for typechecking contracts. The points of entry
;; are two functions check-contract-app and check-contract.

;; Each type rule for contracts proceeds by finding annotated pieces of syntax
;; in the expansion. The typed versions of the contract forms (those in
;; (base-env contract-prims)) annotate the surface syntax before delegating to
;; the untyped form. The typechecking rules in this unit are where we finally
;; use those annotations.

(require racket/match
         racket/list
         racket/function
         racket/sequence
         syntax/parse
         "../utils/utils.rkt"
         (env type-alias-helper type-env-structs lexical-env)
         (types subtype abbrev tc-result match-expanders union numeric-tower
                pairwise-intersect)
         (typecheck check-below)
         (only-in (infer infer) meet join)
         (utils tc-utils contract-utils)
         (rep type-rep values-rep)
         (private syntax-properties)
         "signatures.rkt")

(import tc-expr^)
(export check-contract^)

(define (check-contract-app ctc to-protect [expected #f])
  (define ctc-ty (coerce-to-con (tc-expr/t ctc)))
  (define val-ty (tc-expr/t to-protect))
  (check-below val-ty (Con*-in-ty ctc-ty))
  (ret (pairwise-intersect val-ty (Con*-out-ty ctc-ty))))

(define (check-contract form [expected #f])
  (define rule (tr:ctc-property form))
  (match rule
    [(== ->-key) (tc-arrow-contract form)]
    [(== ->i-key) (tc-arrow-i-contract form)]
    [(== and/c-key) (tc-and/c form)]
    [(== or/c-key) (tc-or/c form)]
    [(== list/c-key) (tc-list/c form)]
    [_ (int-err "unknown contract form ~a" rule)]))


(define (tc-arrow-contract form)
  (define arrow-subforms (or (syntax->list form) (list)))
  (when (empty? arrow-subforms)
    (int-err "no subforms for given -> contract form ~a" form))

  (define (is-arrow? stx) (equal? (tr:ctc-property stx) ->-key))
  (define get-dom-prop (mk/get-sub-prop ->-dom-key))
  (define doms
    (append*
     (map
      (λ (form) (trawl-for-doms/rng form get-dom-prop is-arrow?))
      arrow-subforms)))
  (define-values (in-doms out-doms)
    (for/lists (ins outs)
               ([dom (in-list (sort doms < #:key get-dom-prop))])
      (define dom-ty (coerce-to-con (tc-expr/t dom)))
      (values (Con*-in-ty dom-ty) (Con*-out-ty dom-ty))))

  (define get-rng-prop (mk/get-sub-prop ->-rng-key))
  (define rng*
    (append* (map
              (λ (form) (trawl-for-doms/rng form get-rng-prop is-arrow?))
              arrow-subforms)))
  (define-values (in-rngs out-rngs)
    (for/lists (ins outs)
               ([rng (in-list (sort rng* < #:key get-rng-prop))])
      (define rng-ty (coerce-to-con (tc-expr/t rng)))
      (values (Con*-in-ty rng-ty) (Con*-out-ty rng-ty))))

  (ret (-Contract (->* out-doms (-values in-rngs))
                  (->* in-doms (-values out-rngs)))))

;; trawl-for-doms/rng : syntax predicate predicate -> (listof syntax) Finds
;; syntax/subforms for which is-dom/rng? returns a non-#f value. Does not recur
;; into arrow subforms, according to is-arrow?, since those must still be
;; typechecked, and we do not want to mix the components of one arrow contract
;; with the components of another.
;; NOTE: Clients should not call with a form that is both is-dom/rng? and
;; is-arrow?. In that case, this will simply return a list containing the given
;; form and not its subforms. If they try to typecheck the result thinking it's
;; a *subform*, they will end up in an infinite loop between the arrow
;; typechecking rule and the higher-level typechecker.
(define (trawl-for-doms/rng form is-dom/rng? is-arrow?)
  #;(pretty-print (syntax->datum form))
  (syntax-parse form
    [_
     #:when (is-dom/rng? form)
     (list form)]
    [(forms ...)
     (define-values (arrows non-arrows)
       (for/fold ([arrows '()]
                  [non-arrows '()])
                 ([form (in-list (syntax->list #'(forms ...)))])
         (syntax-parse form
           [_
            ;; we only want arrows that match the predicate; e.g. when looking
            ;; for the rng we have to make sure we don't grab a dom
            #:when (and (is-arrow? form) (is-dom/rng? form))
            (values (cons form arrows) non-arrows)]
           [_
            #:when (is-arrow? form)
            (values arrows non-arrows)]
           [_ (values arrows (cons form non-arrows))])))
     (for/fold ([doms/rng arrows])
               ([non-arrow (in-list non-arrows)])
       (append doms/rng (trawl-for-doms/rng non-arrow is-dom/rng? is-arrow?)))]
    [_ '()]))


;; The function tc-arrow-i-contract typechecks the expansion of the TR's ->i
;; macro. That macro annotates each sub-contract's RHS (e.g. the actual
;; domain/range contract) with its surface identifier, its relative position in
;; the function signature, whether it's mandatory, as well as a few more details
;; before delegating to Racket's ->i. tc-arrow-i-contract uses that information
;; to recover the order of each sub-contract so that it can typecheck them in
;; the correct order and determine the contract's type.

;; What makes this type rule difficult is that the typed ->i macro only sees
;; each contract's and its dependencies' surface syntax, which means that's all
;; it can put in the annotations for this function to recover---during
;; typechecking, though, all of the dependencies' identifiers in ->i's expansion
;; are not necessarily the same as the surface identifiers stored in the
;; annotation. To use the information from the typed ->i macro we must relate
;; the surface identifiers to the expanded identifiers.

;; Consider the following piece of the domain from an ->i use, [x (d1 d2) ctc],
;; which has name x, dependencies d1 and d2, and contract ctc. To support
;; correlating the surface dependency identifiers with the expanded identifiers,
;; TR's ->i macro rewrites ctc to (begin d1 d2 ctc) before delegating. This
;; allows the type rule to retrieve the expanded d1 and d2 and correlate them
;; with the surface versions stored in the annotation, at the cost of leaking
;; the result of this rewrite to the user when ->i raises errors about syntax
;; and contract violations.

(define (tc-arrow-i-contract form)
  (define arrow-subforms (or (syntax->list form) (list)))
  (when (empty? arrow-subforms)
    (int-err "no subforms for given ->i form ~a" form))
  (define (is-arrow-i? stx)
    (equal? (tr:ctc-property stx) ->i-key))
  (define get-dom-prop (mk/get-sub-prop ->i-dom-key))
  (define doms
    (remove-duplicates
     (append*
      (map
       (λ (form) (trawl-for-doms/rng form get-dom-prop is-arrow-i?))
       arrow-subforms))
     equal?
     #:key (lambda (ctc) (dom-info-type (get-dom-prop ctc)))))
  (define get-rest-prop (mk/get-sub-prop ->i-rest-key))
  (define rest-ctc/#f
    (match (trawl-for-doms/rng form get-rest-prop is-arrow-i?)
      [(list rest-ctc) rest-ctc]
      [_ #f]))

  (define dom-ctc->id (compose dom-info-id get-dom-prop))
  (define dom-ctc->deps (compose dom-info-deps get-dom-prop))

  ;; rest-ctc->dom-ctc produces a dom-info struct for the given rest contract's
  ;; rest-info. Useful for typechecking rest contracts which may depend on/be
  ;; depended on by the dom and rng contracts. Treating it as an extra dom
  ;; contract lets us typecheck the whole lot uniformly
  (define (rest-ctc->dom-ctc rest-ctc)
    (define rest (get-rest-prop rest-ctc))
    (tr:ctc-sub-property
     rest-ctc
     (cons
      ->i-dom-key
      (dom-info
      (rest-info-id rest)
      (rest-info-deps rest)
      (rest-info-ctc rest)
      +inf.0
      #t))))
  (define get-rng-prop (mk/get-sub-prop ->i-rng-key))
  (define rngs
    (remove-duplicates
     (append*
      (map
       (λ (form) (trawl-for-doms/rng form get-rng-prop is-arrow-i?))
       arrow-subforms))
     =
     #:key (lambda (ctc) (rng-info-index (get-rng-prop ctc)))))
  (when (zero? (length rngs))
    (int-err "no range contract found when typechecking ->i expansion"))
  (define dom? get-dom-prop)
  (define rng-ctc->id (compose rng-info-id get-rng-prop))
  (define rng-ctc->deps (compose rng-info-deps get-rng-prop))
  (define (dom/rng-ctc->id ctc)
    (if (dom? ctc)
        (dom-ctc->id ctc)
        (rng-ctc->id ctc)))
  (define (dom/rng-ctc->deps ctc)
    (if (dom? ctc)
        (dom-ctc->deps ctc)
        (rng-ctc->deps ctc)))
  (define ctcs (append doms rngs))
  (define-values (topo-sorted-dom-ctcs topo-sorted-rng-ctcs)
    (partition
     dom?
     (topo-sort-ctcs (if rest-ctc/#f
                         (cons (rest-ctc->dom-ctc rest-ctc/#f) ctcs)
                         ctcs)
                     dom/rng-ctc->id
                     dom/rng-ctc->deps)))

  ;; check-subcontract : Stx (Stx -> Listof Stx) Env -> Type/c
  ;; Calculates the Con* type of ctc. All of ctc's dependencies must have their
  ;; surface id mapped to their Con* type in env. Uses ctc->deps to get the
  ;; deps' surface ids so that, after retrieving the deps' expanded ids from the
  ;; the (begin ...)-rewritten contract, it can update env with the appropriate
  ;; type for each expanded id based on its Con* type.
  (define (check-subcontract ctc ctc->deps env)
    (define surface-deps (or (ctc->deps ctc) (list)))
    (define-values (expanded-deps expanded-ctc)
      (syntax-parse ctc
        [(_ deps ... ctc)
         (values #'(deps ...) #'ctc)]))
    (define lookup-fail (mk/lookup-fail-in "deps-env"))
    (define deps-env
      (for/fold ([env env])
                ([surface-id surface-deps]
                 [expanded-id (in-syntax expanded-deps)])
        (env-set-id-type env expanded-id (Con*-out-ty (env-lookup-id env surface-id lookup-fail)))))
    (with-lexical-env deps-env
      (coerce-to-con (tc-expr/t expanded-ctc))))

  (define doms-checked-env
    (for/fold ([env (lexical-env)])
              ([ctc topo-sorted-dom-ctcs])
      (define ctc-ty (check-subcontract ctc dom-ctc->deps env))
      (env-set-id-type env (dom-ctc->id ctc) ctc-ty)))

  (define rest-ctc-ty/#f
    (match (and rest-ctc/#f
                (env-lookup-id
                 doms-checked-env
                 (rest-info-id (get-rest-prop rest-ctc/#f))
                 (mk/lookup-fail-in "doms-checked-env")))
      [#f #f]
      [(and t (Contract*: (Listof: _) (Listof: _))) t]
      [non-list-ty
       (tc-error/fields
        "#:rest contract must be a list contract"
        #:delayed? #t
        "#:rest contract type" non-list-ty)
       (-Contract (make-Listof Univ) (make-Listof (Un)))]))

  ;; Calculates the type of the pre/post-condition expression. Exactly like
  ;; check-subcontract, each pre/post-condition must have its dep's surface id's
  ;; mapped to their Con* type in env.
  (define (check-pre/post expr env expr->deps expr->desc?)
    (define-values (expanded-deps expanded-expr)
      (syntax-parse expr
        [(_ () deps ... expr)
         (values #'(deps ...) #'expr)]))
    (define lookup-fail (mk/lookup-fail-in "pre/post deps-env"))
    (define surface-deps (or (expr->deps expr) (list)))
    (define deps-env
      (for/fold ([env env])
                ([surface-id surface-deps]
                 [expanded-id (in-syntax expanded-deps)])
        (env-set-id-type env expanded-id (Con*-out-ty (env-lookup-id env surface-id lookup-fail)))))
    (with-lexical-env deps-env
      (tc-expr/check expanded-expr (ret
                                    (if (expr->desc? expr)
                                        (Un -Boolean -String (-lst -String))
                                        Univ)))))

  (define get-pre-prop (mk/get-sub-prop ->i-pre-key))
  (define is-pre? get-pre-prop)
  (define (pre-dont-recur? form)
    (or (is-arrow-i? form) (is-pre? form)))
  (define pres
    (append*
     (map
      (λ (form) (trawl-for-subs form pre-dont-recur? is-pre?))
      arrow-subforms)))
  (define pre->deps (compose pre-info-deps get-pre-prop))
  (define pre->position (compose pre-info-position get-pre-prop))
  (define pre->desc? (compose pre-info-desc? get-pre-prop))
  (for-each (λ (p) (check-pre/post p doms-checked-env pre->deps pre->desc?))
            (sort pres < #:key pre->position))

  (define rng-checked-env
    (for/fold ([env doms-checked-env])
              ([ctc topo-sorted-rng-ctcs])
      (define ctc-ty (check-subcontract ctc rng-ctc->deps env))
      (env-set-id-type env (rng-ctc->id ctc) ctc-ty)))
  (define rng-ctcs (sort rngs < #:key (compose rng-info-index get-rng-prop)))
  (define-values (rng-in-tys rng-out-tys)
    (for/lists (in-tys out-tys)
               ([ctc rng-ctcs])
      (define lookup-fail (mk/lookup-fail-in "rng-checked-env"))
      (define ctc-ty (check-subcontract ctc rng-ctc->deps rng-checked-env))
      (values (Con*-in-ty ctc-ty) (Con*-out-ty ctc-ty))))

  (define get-post-prop (mk/get-sub-prop ->i-post-key))
  (define is-post? get-post-prop)
  (define (post-dont-recur? form)
    (or (is-arrow-i? form) (is-post? form)))
  (define posts
    (append*
     (map
      (λ (form) (trawl-for-subs form post-dont-recur? is-post?))
      arrow-subforms)))
  (define post-infos (map get-post-prop posts))
  (define post->deps (compose post-info-deps get-post-prop))
  (define post->desc? (compose post-info-desc? get-post-prop))
  (define post->position (compose post-info-position get-post-prop))
  (for-each (λ (p) (check-pre/post p rng-checked-env post->deps post->desc?))
            (sort posts < #:key post->position))


  (define dom-infos (map get-dom-prop doms))
  (define-values (kw-doms plain-doms)
    (partition (compose keyword? dom-info-type) dom-infos))
  (define-values (reqd-plain-doms opt-plain-doms)
    (partition dom-info-mandatory? (sort plain-doms < #:key dom-info-type)))
  (define sorted-kw-doms (sort kw-doms keyword<? #:key dom-info-type))
  (define opt-count (length opt-plain-doms))
  (define-values (in-arrs out-arrs)
    (for/lists (ins outs)
               ([vararg-slice-length (in-range (add1 opt-count))])
      (define opts (take opt-plain-doms vararg-slice-length))
      (define doms (append reqd-plain-doms opts))
      (define lookup-fail (mk/lookup-fail-in "doms-checked-env"))
      (define (dom-ty d) (env-lookup-id doms-checked-env (dom-info-id d) lookup-fail))
      (define ((kw-in/out Con*in/out-ty) kw-info)
        (define kw (dom-info-type kw-info))
        (define ty (env-lookup-id doms-checked-env (dom-info-id kw-info) lookup-fail))
        (make-Keyword kw (Con*in/out-ty ty) (dom-info-mandatory? kw-info)))
      (define (list-contents-ty list-ty)
        (match list-ty
          [(Listof: ty) ty]))
      (values
       (-Arrow (map (compose Con*-out-ty dom-ty) doms)
               (make-Values (map (λ (ty) (-result ty -tt-propset -empty-obj)) rng-in-tys))
               #:rest (and rest-ctc-ty/#f
                           ;; at this point, we know rest-ctc-ty/#f will be
                           ;; given a list type
                           (list-contents-ty (Con*-out-ty rest-ctc-ty/#f)))
               #:kws (map (kw-in/out Con*-out-ty) sorted-kw-doms))
       (-Arrow (map (compose Con*-in-ty dom-ty) doms)
               (make-Values (map (λ (ty) (-result ty -tt-propset -empty-obj)) rng-out-tys))
               #:rest (and rest-ctc-ty/#f
                           ;; ditto above remark about rest-ctc-ty/#f
                           (list-contents-ty (Con*-in-ty rest-ctc-ty/#f)))
               #:kws (map (kw-in/out Con*-in-ty) sorted-kw-doms)))))
  (ret (-Contract (make-Fun in-arrs) (make-Fun out-arrs))))

;; topo-sort-ctcs : (Listof Stx) (Stx -> Id) (Stx -> Listof Id) -> Listof Stx
;; Returns a permutation of ctcs in topo-order, according to their dependencies
(define (topo-sort-ctcs ctcs ctc->id ctc->deps)
  (define dep-map (for/list ([ctc ctcs])
                    (define surface-id (ctc->id ctc))
                    (define deps (or (ctc->deps ctc) (list)))
                    (cons surface-id deps)))
  (define sorted-ids* (flatten (find-strongly-connected-type-aliases dep-map)))
  (define topo-sorted-ids (reverse sorted-ids*))

  (define ((ctc-matcher-for-id id) ctc) (free-identifier=? id (ctc->id ctc)))
  (for/list ([id topo-sorted-ids])
    (findf (ctc-matcher-for-id id) ctcs)))

(define ((mk/lookup-fail-in name) id)
  (int-err (format "couldn't find ~a in ~a" id name)))

(define ((mk/get-sub-prop key) stx)
  (match (tr:ctc-sub-property stx)
    [(cons (== key) value) value]
    [_ #f]))

;; trawl-for-subs : syntax -> (list syntax)
;; Don't call with a dont-recur? that is also is-sub?; similar reason as with
;; trawl-for-doms/rng
(define (trawl-for-subs form dont-recur? is-sub?)
  (syntax-parse form
    [_
     #:when (is-sub? form)
     (list form)]
    [(forms ...)
     (for/fold ([subs '()])
               ([form (in-list (syntax->list #'(forms ...)))])
       (syntax-parse form
         [_
          #:when (and (dont-recur? form)
                      (is-sub? form))
          (cons form subs)]
         [_ (append subs (trawl-for-subs form dont-recur? is-sub?))]))]
    [_ '()]))


(define (tc-and/c form)
  (define subforms (or (syntax->list form) (list)))
  (when (empty? subforms)
    (int-err "no subforms for given and/c form ~a" form))
  (define (is-and/c? stx) (equal? (tr:ctc-property stx) and/c-key))
  (define get-index (mk/get-sub-prop and/c-index-key))
  (define subs (sort (trawl-for-subs subforms
                                     is-and/c?
                                     (conjoin syntax? get-index))
                      <
                      #:key get-index))

  (define-values (in-ty out-ty)
    (match subs
      [(list sub0 subs-rest ...)
       (define sub0-ty (coerce-to-con (tc-expr/t sub0)))
       (for/fold ([in-ty (Con*-in-ty sub0-ty)]
                  [out-ty (Con*-out-ty sub0-ty)])
                 ([sub (in-list subs-rest)])
         (parameterize ([current-orig-stx sub])
           (define ty (coerce-to-con (tc-expr/t sub)))
           (check-below out-ty (Con*-in-ty ty))
           (values in-ty (pairwise-intersect out-ty (Con*-out-ty ty)))))]
      [(list) (values Univ Univ)]))
  (ret (-Contract in-ty out-ty)))

(define (tc-or/c form)
  (define subforms (or (syntax->list form) (list)))
  (when (empty? subforms)
    (int-err "no subforms for given or/c form ~a" form))
  (define (is-or/c? stx) (equal? (tr:ctc-property stx) or/c-key))
  (define get-index (mk/get-sub-prop or/c-index-key))
  (define subs (sort (trawl-for-subs subforms
                                     is-or/c?
                                     (conjoin syntax? get-index))
                     <
                     #:key get-index))
  (define-values (in-ty out-ty)
    (for/fold ([in-ty Univ]
               [out-ty (Un)])
              ([sub (in-list subs)])
      (define con-ty (coerce-to-con (tc-expr/t sub)))
      (values
       (meet in-ty (Con*-in-ty con-ty))
       (join out-ty (Con*-out-ty con-ty)))))
  (ret (-Contract in-ty out-ty)))

(define (tc-list/c form)
  (define (is-list/c? stx) (equal? (tr:ctc-property stx) list/c-key))
  (define get-index (mk/get-sub-prop list/c-index-key))
  (define subs
    (match (syntax->list form)
      [(list subforms ...)
       (define subs* (trawl-for-subs subforms is-list/c?
                                     (conjoin syntax? get-index)))
       (sort subs* < #:key get-index)]
      [#f (int-err "no subforms for given list/c form ~a" form)]))

  (define-values (in-tys out-tys)
    (for/lists (ins outs)
               ([sub (in-list subs)])
      (define con-ty (coerce-to-con (tc-expr/t sub)))
      (values (Con*-in-ty con-ty) (Con*-out-ty con-ty))))
  (ret (match* (in-tys out-tys)
         [((list) (list))
          (-Contract Univ (-lst*))]
         [((list _ _ ...) (list _ _ ...))
          (-Contract (apply -lst* in-tys) (apply -lst* out-tys))])))


(define (coerce-to-con ty)
  (define coercible-simple-value-types
    (Un -Null -Symbol -Boolean -Keyword -Char -Bytes -String -Number))
  [match ty
    [(Contract*: t _) ty]
    [(ConFn*: in-ty out-ty)
     (-FlatContract in-ty out-ty)]
    [_
     #:when (subtype ty coercible-simple-value-types)
     (-FlatContract Univ ty)]
    ;; Because the type of these isn't the core type needed for the contract,
    ;; they need to be handled differently than coercible-simple-value-types
    [(== -Regexp) (-FlatContract Univ -String)]
    [(== -Byte-Regexp) (-FlatContract Univ -Bytes)]
    [_ (tc-error/fields "could not coerce to a contract type"
                        #:delayed? #t
                        "type" ty)
       (-Contract Univ (Un))]])
