#lang racket/base
;; This module provides typed versions of identifiers from racket/contract (not
;; to be confused with prims-contract).

;; Many contract forms (e.g. >/c) can be directly ascribed a type, which is done
;; using the define-contract macro. Others, though, have more complicated
;; typings and so the typed version annotates the typechecking-relevant pieces
;; of surface syntax before delegating to the untyped form. The annotated pieces
;; are retrieved by the form's type rule, after the form has fully expanded.

(require "../utils/utils.rkt"
         (for-syntax racket/base
                     racket/sequence
                     racket/syntax
                     racket/list
                     syntax/parse
                     syntax/transformer
                     (utils contract-utils)
                     (private syntax-properties))
         ;; these types go into ignore-some-expr-property, which must contain
         ;; syntax objects for surface-level types (e.g. Real instead of -Real)
         (base-env base-types base-types-extra)
         (prefix-in untyped: racket/contract/base))
(provide (except-out (all-defined-out)
                     define-contract)
         ;; TODO: since we don't support all of the p/c-item forms (exists,
         ;; struct, etc.), we really should provide our own versions of these
         ;; two forms that give good "x is unsupported" messages
         (rename-out [untyped:provide/contract provide/contract]
                     [untyped:contract-out contract-out]
                     [untyped:contract contract]))

(define-syntax (define-contract stx)
  (define-syntax-class def
    (pattern [ctc:identifier ty]
             #:with untyped-ctc (format-id stx "untyped:~a" #'ctc)))
  (syntax-parse stx
    [(_  :def ...)
     #'(begin
         (define-syntax ctc
           (make-variable-like-transformer
            #`(#,(ignore-some-expr-property #'#%expression #'ty)
               untyped-ctc))) ...)]))

(define-contract
  [flat-named-contract (All (a b) (-> Any (FlatCon a b) (FlatCon a b)))]
  [any/c (FlatCon Any Any)]
  [none/c (FlatCon Any Any)]
  [not/c (All (a b) (-> (FlatCon a b) (FlatCon a a)))]
  [=/c (case-> (-> Natural (FlatCon Any Natural))
               (-> Integer (FlatCon Any Integer))
               (-> Real (FlatCon Any Real)))]
  [</c (-> Real (FlatCon Any Real))]
  [>/c (-> Real (FlatCon Any Real))]
  [<=/c (-> Real (FlatCon Any Real))]
  [>=/c (-> Real (FlatCon Any Real))]
  [between/c (-> Real Real (FlatCon Any Real))]
  [real-in (-> Real Real (FlatCon Any Real))]
  [integer-in
   (case-> (-> Positive-Integer Integer (FlatCon Any Positive-Integer))
           (-> Natural Integer (FlatCon Any Natural))
           (-> Integer Integer (FlatCon Any Integer)))]
  [natural-number/c (FlatCon Any Natural)]
  [string-len/c (-> Real (FlatCon Any String))]
  ;; Because we can use FlatCon in function position and because false/c = #f,
  ;; giving it the type below is not sound
  ;; [false/c (FlatCon Any False)]
  [printable/c (FlatCon Any Any)]
  ;; one-of/c
  ;; vectorof
  ;; vector-immutableof (tricky, TR doesn't have immutable vectors)
  ;; vector/c
  ;; vector-immutable/c
  ;; box/c
  ;; box-immutable/c
  [listof (All (a b) (-> (Con a b) (Con (Listof a) (Listof b))))]
  [non-empty-listof
   (All (a b) (-> (Con a b) (Con (Listof a) (Pairof b (Listof b)))))]
  [list*of
   (All (a b) (-> (Con a b) (Con (Rec x (Pairof a (U a x)))
                                 (Rec x (Pairof b (U b x))))))]
  [cons/c (All (a b c d) (-> (Con a b) (Con c d) (Con Any (Pairof b d))))]
  ;; cons/dc
  [syntax/c (All (a b) (-> (FlatCon a b) (FlatCon (Syntaxof a) (Syntaxof b))))]
  ;; struct/c
  ;; struct/dc
  [parameter/c
   (All (a b c d) (case-> (-> (Con a b) (Con (Parameter b) (Parameter b)))
                          (-> (Con a b) (Con c d) (Con (Parameter b d)
                                                       (Parameter b d)))))]
  ;; procedure-arity-includes/c
  ;; hash/c
  ;; hash/dc
  ;; channel/c
  ;; prompt-tag/c
  ;; continuation-mark-key/c
  ;; evt/c
  ;; flat-rec-contract
  ;; flat-murec-contract
  ;; any
  ;; promise/c
  ;; flat-contract
  ;; flat-contract-predicate
  [symbols (-> Symbol Symbol * (Con Any Symbol))])

(define-syntax (and/c stx)
  (syntax-parse stx
    #:literals (and/c)
    [(and/c ctc:expr ...)
     (tr:ctc-property
      (quasisyntax/loc stx
        (untyped:and/c #,@(for/list ([ctc (in-syntax #'(ctc ...))]
                                     [idx (in-naturals)])
                            (tr:ctc-sub-property ctc (cons and/c-index-key idx)))))
      and/c-key)]))

(define-syntax (or/c stx)
  (syntax-parse stx
    #:literals (or/c)
    [(or/c ctc:expr ...)
     (tr:ctc-property
      (quasisyntax/loc stx
        (untyped:or/c #,@(for/list ([ctc (in-syntax #'(ctc ...))]
                                    [idx (in-naturals)])
                           (tr:ctc-sub-property ctc (cons or/c-index-key idx)))))
      or/c-key)]))

;; list/c needs its own type rule because giving it a function type outright
;; wouldn't allow us to give e.g. (list/c exact-integer? positive?) a type like
;; (Con (List Any Real) (List Integer Positive-Real)). The form is polyvariadic
;; but polydots is currently too weak, it won't let us give list/c a type like
;; (All ((a b) ...) (-> (Con a b) ... (Con (List a ...) (List b ...)))).
(define-syntax (list/c stx)
  (syntax-parse stx
    #:literals (list/c)
    ;; TODO: add a case for higher-order use of list/c---it's a procedure
    [(list/c ctc:expr ...)
     (tr:ctc-property
      (quasisyntax/loc stx
        (untyped:list/c #,@(for/list ([ctc (in-syntax #'(ctc ...))]
                                      [idx (in-naturals)])
                             (tr:ctc-sub-property ctc (cons list/c-index-key idx)))))
      list/c-key)]))

(define-syntax (->/c stx)
  (syntax-parse stx
    #:literals (->/c)
    [(->/c doms:expr ... ((~literal values) rngs:expr ...))
     (tr:ctc-property
      (ignore
       (quasisyntax/loc stx
         (untyped:-> #,@(for/list ([dom (in-syntax #'(doms ...))]
                                   [idx (in-naturals)])
                          (tr:ctc-sub-property dom (cons ->-dom-key idx)))
                     (values #,@(for/list ([rng (in-syntax #'(rngs ...))]
                                           [idx (in-naturals)])
                                  (tr:ctc-sub-property rng (cons ->-rng-key idx)))))))
      ->-key)]
    [(->/c doms:expr ... (~or (~and (~literal any)
                                    ~!
                                    (~fail "Typed Racket does not support the any range"))
                              rng:expr))
     (tr:ctc-property
      (ignore
       (quasisyntax/loc stx
         (untyped:-> #,@(for/list ([dom (in-syntax #'(doms ...))]
                                   [idx (in-naturals)])
                          (tr:ctc-sub-property dom (cons ->-dom-key idx)))
                     #,(tr:ctc-sub-property #'rng (cons ->-rng-key 0)))))
      ->-key)]))

(define-syntax (->i stx)
  (define-syntax-class id+ctc
    #:attributes ((deps 1) id ctc)
    (pattern [id:id ctc]
             #:attr (deps 1) #f)
    (pattern [id:id (deps:id ...) ctc]))
  (define dom-counter 0)
  (define (next-dom-index)
    (begin0 dom-counter
      (set! dom-counter (add1 dom-counter))))
  (define-splicing-syntax-class (dom mandatory?)
    #:attributes (form)
    (pattern (~seq (~optional kw:keyword) dom:id+ctc)
             #:attr form (begin
                           (define info
                             (dom-info #'dom.id
                                       (attribute dom.deps)
                                       #'dom.ctc
                                       (if (attribute kw)
                                           (syntax-e (attribute kw))
                                           (next-dom-index))
                                       mandatory?))
                           (define deps-to-splice (if (dom-info-deps info)
                                                      (list (dom-info-deps info))
                                                      (list)))
                           (define kw-to-splice (if (keyword? (dom-info-type info))
                                                    (list (dom-info-type info))
                                                    (list)))
                           #`(#,@kw-to-splice
                              (dom.id
                               #,@deps-to-splice
                               #,(tr:ctc-sub-property
                                  ;; ->i doesn't preserve the syntax properties if we
                                  ;; put this on the id+ctc pair; it also doesn't
                                  ;; guarantee that the identifier for the named dom
                                  ;; position will end up in the expanded syntax. The
                                  ;; ctc is most likely to be in the expansion, so
                                  ;; we'll put all the properties there
                                  #`(begin #,@(or (attribute dom.deps) (list)) dom.ctc)
                                  (cons ->i-dom-key info)))))))
  ;; TODO: parameterized syntax class for streamlining pre/post conditions
  (define pre-counter 0)
  ;; pre-condition->forms : Syntax Syntax #:desc? [Option<Boolean>] #:name [Option<String>] -> Listof<(U Syntax Listof<Syntax>)>
  (define (pre-condition->forms deps condition #:desc? [desc? #f] #:name [name #f])
    (define kw
      (cond
        [desc? #'#:pre/desc]
        [name #'#:pre/name]
        [else #'#:pre]))
    (list kw
          deps
          (if name (list name) (list))
          (tr:ctc-sub-property
           #`(let () #,@deps #,condition)
           (cons ->i-pre-key
                 (pre-info (syntax->list deps)
                           (begin0 pre-counter
                             (set! pre-counter (add1 pre-counter)))
                           desc?)))))
  (define-splicing-syntax-class pre-condition
    #:attributes (forms)
    (pattern (~seq #:pre (deps:id ...) condition)
             #:attr forms
             (pre-condition->forms #'(deps ...) #'condition))
    (pattern (~seq #:pre/desc (deps:id ...) condition)
             #:attr forms
             (pre-condition->forms #'(deps ...) #'condition #:desc? #t))
    (pattern (~seq #:pre/name (deps:id ...) name:str condition)
             #:attr forms
             (pre-condition->forms #'(deps ...) #'condition #:name #'name)))
  (define post-counter 0)
  ;; post-condition->form : Syntax Syntax #:desc? [Option<Boolean>] #:name [Option<String>] -> Listof<(U Syntax Listof<Syntax>)>
  (define (post-condition->forms deps condition #:desc? [desc? #f] #:name [name #f])
    (define kw
      (cond
        [desc? #'#:post/desc]
        [name #'#:post/name]
        [else #'#:post]))
    (list kw
          deps
          (if name (list name) (list))
          (tr:ctc-sub-property
           ;; we use an empty let instead of a begin because the begin
           ;; was getting expanded away, making it much harder to
           ;; analyze the deps and condition as a single unit
           #`(let () #,@deps #,condition)
           (cons ->i-post-key
                 (post-info (syntax->list deps)
                            (begin0 post-counter
                              (set! post-counter (add1 post-counter)))
                            desc?)))))
  (define-splicing-syntax-class post-condition
    #:attributes (forms)
    (pattern (~seq #:post (deps:id ...) condition)
             #:attr forms
             (post-condition->forms #'(deps ...) #'condition))
    (pattern (~seq #:post/desc (deps:id ...) condition)
             #:attr forms
             (post-condition->forms #'(deps ...) #'condition #:desc? #t))
    (pattern (~seq #:post/name (deps:id ...) name:str condition)
             #:attr forms
             (post-condition->forms #'(deps ...) #'condition #:name #'name)))
    
  (define rng-counter 0)
  ;; MUTATES: modifies rng-counter
  (define (rng-id+ctc->form rng)
    (syntax-parse rng
      [rng:id+ctc
       (define index (+ rng-counter 1))
       (set! rng-counter index)
       (define info (rng-info #'rng.id (attribute rng.deps) #'rng.ctc index))
       (define new-ctc #`(begin #,@(or (attribute rng.deps) (list)) rng.ctc))
       #`[rng.id
          #,@(or (and (attribute rng.deps) (list (attribute rng.deps)))
                 (list))
          #,(tr:ctc-sub-property new-ctc (cons ->i-rng-key info))]]))
  (define-syntax-class dependent-range
    #:attributes (form)
    (pattern (~literal any)
             #:attr form
             (tr:ctc-sub-property
              #'untyped:any
              (cons ->i-rng-key (rng-info #f #f #'any 0)))
             ;; TODO: support this
             #:fail-when #t "Typed Racket does not support the any range")
    (pattern ((~literal values) rng:id+ctc ...)
             #:attr form
             #`(values #,@(map rng-id+ctc->form (syntax->list #'(rng ...)))))
    ;; TODO: this overlaps with the (values id+ctc ...) case, so the order of
    ;; these matters -- fix that? is it cleaner to just note the order matters?
    (pattern rng:id+ctc
             #:attr form
             (rng-id+ctc->form #'rng)))
  (define-splicing-syntax-class dependent-rest
    #:attributes ((deps 1) ctc id)
    (pattern (~seq #:rest :id+ctc)))

  (syntax-parse stx
    #:literals (->i)
    [(->i ((~var mand-doms (dom #t)) ...)
          (~optional ((~var opt-doms (dom #f)) ...))
          (~optional rest:dependent-rest)
          (~optional (~seq pre:pre-condition ...))
          rng:dependent-range
          ;; TODO: post-conditions are only allowed when dependent-range is
          ;; non-any
          (~optional (~seq post:post-condition ...)))
     (tr:ctc-property
      (ignore
       (quasisyntax/loc stx
         (untyped:->i (#,@(apply append (map syntax->list (or (attribute mand-doms.form)
                                                              (list)))))
                      #,@(let ()
                           (define opt-doms/#f (attribute opt-doms.form))
                           (if (not opt-doms/#f)
                               (list)
                               (list (apply append (map syntax->list opt-doms/#f)))))
                      #,@(flatten (or (attribute pre.forms) (list)))
                      #,@(if (attribute rest)
                             (list #'#:rest #`[rest.id
                                               #,@(or (and (attribute rest.deps) (list (attribute rest.deps)))
                                                      (list))
                                               #,(tr:ctc-sub-property
                                                  #`(begin #,@(or (attribute rest.deps) (list))
                                                           rest.ctc)
                                                  (cons ->i-rest-key
                                                        (rest-info #'rest.id
                                                                   (or (attribute rest.deps)
                                                                       (list))
                                                                   #'rest.ctc)))])
                             (list))
                      rng.form
                      #,@(flatten (or (attribute post.forms) (list))))))
      ->i-key)]))

