#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/sequence racket/set racket/list
         (only-in racket/list make-list)
         (contract-req)
         (typecheck check-below tc-subst tc-metafunctions possible-domains)
         (utils tc-utils)
         (rep type-rep filter-rep)
         (except-in (types utils abbrev subtype type-table)
                    -> ->* one-of/c))
(require-for-cond-contract
  syntax/stx)

(provide/cond-contract
  [tc/funapp1
    ((syntax? stx-list? arr? (listof tc-results/c) (or/c #f tc-results/c))
     (#:check boolean?)
     . ->* . full-tc-results/c)])
(define (tc/funapp1 f-stx args-stx ftype0 argtys expected #:check [check? #t])
  ;; update tooltip-table with inferred function type
  (add-typeof-expr f-stx (ret (make-Function (list ftype0))))
  (match* (ftype0 argtys)
    ;; we check that all kw args are optional
    [((arr: dom rng rest #f (and kws (list (Keyword: _ _ #f) ...)))
      (list (tc-result1: t-a phi-a o-a) ...))

     (when check?
       (cond [(and (not rest) (not (= (length dom) (length t-a))))
              (tc-error/fields "could not apply function"
                               #:more "wrong number of arguments provided"
                               "expected" (length dom)
                               "given" (length t-a)
                               #:delayed? #t)]
             [(and rest (< (length t-a) (length dom)))
              (tc-error/fields "could not apply function"
                               #:more "wrong number of arguments provided"
                               "expected at least" (length dom)
                               "given" (length t-a)
                               #:delayed? #t)])
       (for ([dom-t (if rest (in-sequence-forever dom rest) (in-list dom))]
             [a (in-syntax args-stx)]
             [arg-t (in-list t-a)])
         (parameterize ([current-orig-stx a]) (check-below arg-t dom-t))))
     (let* ([dom-count (length dom)])
       ;; Currently do nothing with rest args and keyword args as there are no support for them in
       ;; objects yet.
       (let-values
           ([(o-a t-a) (for/lists (os ts)
                         ([_ (in-range dom-count)]
                          [oa (in-sequence-forever (in-list o-a) -empty-obj)]
                          [ta (in-sequence-forever (in-list t-a) Univ)])
                         (values oa ta))])
           (values->tc-results rng o-a t-a)))]
    ;; this case should only match if the function type has mandatory keywords
    ;; but no keywords were provided in the application
    [((arr: _ _ _ _
            ;; at least one mandatory keyword
            (app (λ (kws)
                   (for/or ([keyword (in-list kws)])
                     (match keyword
                       [(Keyword: kw _ #t) kw]
                       [_ #f])))
                 (? values req-kw))) _)
     (when check?
       (tc-error/fields "could not apply function"
                        #:more "a required keyword was not supplied"
                        "missing keyword" req-kw))]
    [((arr: _ _ _ drest '()) _)
     (int-err "funapp with drest args ~a ~a NYI" drest argtys)]
    [((arr: _ _ _ _ kws) _)
     (int-err "funapp with keyword args ~a NYI" kws)]))


(define (make-printable t)
  (match t
    [(tc-result1: t) (cleanup-type t)]
    [(or (tc-results: ts)
         (tc-results: ts _ _ _ _))
     (-values (map cleanup-type ts))]
    [(tc-any-results: f) (-AnyValues -top)]
    [_ t]))

(define (stringify-domain dom rst drst [rng #f])
  (let ([doms-string (if (null? dom) "" (string-append (stringify (map make-printable dom)) " "))]
        [rng-string (if rng (format " -> ~a" rng) "")])
    (cond [drst
           (format "~a~a ... ~a~a" doms-string (car drst) (cdr drst) rng-string)]
          [rst
           (format "~a~a *~a" doms-string rst rng-string)]
          [else (string-append (stringify (map make-printable dom)) rng-string)])))

;; Generates error messages when operand types don't match operator domains.
(provide/cond-contract
  [domain-mismatches
   ((syntax? syntax? Type/c (listof (listof Type/c)) (listof (or/c #f Type/c))
     (listof (or/c #f (cons/c Type/c (or/c natural-number/c symbol?))))
     (listof SomeValues/c) (listof tc-results?) (or/c #f Type/c) any/c)
    (#:expected (or/c #f tc-results/c)
     #:return tc-results?
     #:msg-thunk (-> string? string?))
    . ->* . tc-results/c)])
(define (domain-mismatches f-stx args-stx ty doms rests drests rngs arg-tys tail-ty tail-bound
                           #:expected [expected #f] #:return [return (ret -Bottom)]
                           #:msg-thunk [msg-thunk (lambda (dom) dom)])
  (define arguments-str
    (stringify-domain arg-tys
                      (if (not tail-bound) tail-ty #f)
                      (if tail-bound (cons tail-ty tail-bound) #f)))
  (cond
    [(null? doms)
     (tc-error/expr/fields
      "cannot apply a function with unknown arity"
      #:more (format "~a has type Procedure which cannot be applied"
                     (name->function-str (and (identifier? f-stx) f-stx)))
      #:return return)]
    [(and (= 1 (length doms)) (not (car rests)) (not (car drests)) (not tail-ty) (not tail-bound))
     (tc-error/expr
      #:return return
      (msg-thunk
       (apply string-append
              (if (not (= (length (car doms)) (length arg-tys)))
                  (format "Wrong number of arguments - Expected ~a, but got ~a\n\n" (length (car doms)) (length arg-tys))
                  "")
              (append
               (for/list ([dom-t (in-list (list-extend arg-tys (car doms) #f))]
                          [arg-t (in-list (list-extend (car doms) arg-tys #f))]
                          [i (in-naturals 1)])
                         (let ([dom-t (or dom-t "-none-")]
                               [arg-t (or arg-t "-none-")])
                           (format "Argument ~a:\n  Expected: ~a\n  Given:    ~a\n" i (make-printable dom-t) (make-printable arg-t))))
               (list
                (if expected
                    (format "\nResult type:     ~a\nExpected result: ~a\n"
                            (car rngs) (make-printable expected))
                    ""))))))]
    [(= 1 (length doms))
     (tc-error/expr
      #:return return
      (msg-thunk
       (string-append
        "Domain: "
        (stringify-domain (car doms) (car rests) (car drests))
        "\nArguments: "
        arguments-str
        "\n"
        (if expected
            (format "Result type: ~a\nExpected result: ~a\n"
                    (car rngs) (make-printable expected))
            ""))))]
    [else
     (define label  (if expected   "Types: "   "Domains: "))
     (define nl+spc (if expected "\n       " "\n         "))
     ;; we restrict the domains shown in the error messages to those that
     ;; are useful
     (match-let ([(list pdoms prngs prests pdrests) (possible-domains doms rests drests rngs expected)])
       ;; if we somehow eliminate all the cases (bogus expected type) fall back to showing the
       ;; extra cases
       (let-values ([(pdoms rngs rests drests)
                     (if (null? pdoms)
                         (values doms rngs rests drests)
                         (values pdoms prngs prests pdrests))])
         ;; only use `tc/funapp1` if `tail-ty` was *not* provided
         ;; since it either won't error correctly or produces a poor error
         (cond [(and (not tail-ty)
                     (= (length pdoms) 1)
                     ;; The correctness of selecting this case depends on the
                     ;; domain selection being consistent with the expected
                     ;; type. Since possible-domains only checks this in restrictive
                     ;; mode, do the check here. Note that using restrictive mode
                     ;; above results in poor error messages (see PR 14731).
                     (or (not expected)
                         (subtype (car rngs) (tc-results->values expected))))
                ;; if we narrowed down the possible cases to a single one, have
                ;; tc/funapp1 generate a better error message
                (tc/funapp1 f-stx args-stx
                            (make-arr (car pdoms) (car rngs)
                                      (car rests) (car drests) null)
                            arg-tys expected)
                return]
               [else
                ;; if not, print the message as usual
                (define err-doms
                  (string-append
                   label
                   (stringify (if expected
                                  (map stringify-domain pdoms rests drests rngs)
                                  (map stringify-domain pdoms rests drests))
                              nl+spc)
                   "\nArguments: "
                   arguments-str
                   "\n"
                   (if expected
                       (format "Expected result: ~a\n" (make-printable expected))
                       "")))
                (tc-error/expr
                 #:return return
                 (msg-thunk err-doms))])))])) ; generate message


(provide/cond-contract
  [poly-fail ((syntax? syntax? Type/c (listof tc-results?))
              (#:name (or/c #f syntax?)
               #:expected (or/c #f tc-results/c))
              . ->* . tc-results/c)])
(define (poly-fail f-stx args-stx t argtypes #:name [name #f] #:expected [expected #f])
  (match t
    [(or (Poly-names:
          msg-vars
          (Function/arrs: msg-doms msg-rngs msg-rests msg-drests (list (Keyword: _ _ #f) ...)))
         (PolyDots-names:
          msg-vars
          (Function/arrs: msg-doms msg-rngs msg-rests msg-drests (list (Keyword: _ _ #f) ...)))
         (PolyRow-names:
          msg-vars _
          (Function/arrs: msg-doms msg-rngs msg-rests msg-drests (list (Keyword: _ _ #f) ...))))
     (let ([fcn-string (name->function-str name)])
       (if (and (andmap null? msg-doms)
                (null? argtypes))
           (tc-error/expr (string-append
                           "Could not infer types for applying polymorphic "
                           fcn-string
                           "\n"))
           (domain-mismatches f-stx args-stx t msg-doms msg-rests msg-drests
                              msg-rngs argtypes #f #f #:expected expected
                              #:msg-thunk (lambda (dom)
                                            (string-append
                                             "Polymorphic " fcn-string " could not be applied to arguments:\n"
                                             dom
                                             (if (not (subset? (apply set-union (seteq) (map fv/list msg-doms))
                                                               (list->seteq msg-vars)))
                                                 (string-append "Type Variables: " (stringify msg-vars) "\n")
                                                 ""))))))]
    [(or (Poly-names: msg-vars (Function/arrs: msg-doms msg-rngs msg-rests msg-drests kws))
         (PolyDots-names: msg-vars (Function/arrs: msg-doms msg-rngs msg-rests msg-drests kws))
         (PolyRow-names: msg-vars _ (Function/arrs: msg-doms msg-rngs msg-rests msg-drests kws)))
     (let ([fcn-string (if name
                           (format "function with keywords ~a" (syntax->datum name))
                           "function with keywords")])
       (if (and (andmap null? msg-doms)
                (null? argtypes))
           (tc-error/expr (string-append
                           "Could not infer types for applying polymorphic "
                           fcn-string
                           "\n"))
           (domain-mismatches f-stx args-stx t msg-doms msg-rests msg-drests
                              msg-rngs argtypes #f #f #:expected expected
                              #:msg-thunk (lambda (dom)
                                            (string-append
                                             "Polymorphic " fcn-string " could not be applied to arguments:\n"
                                             dom
                                             (if (not (subset? (apply set-union (seteq) (map fv/list msg-doms))
                                                               (list->seteq msg-vars)))
                                                 (string-append "Type Variables: " (stringify msg-vars) "\n")
                                                 ""))))))]))

;; name->function-str : (Option Identifier) -> String
;; Produce a function name string for error messages
(define (name->function-str name)
  (if name
      (format "function `~a'" (syntax->datum name))
      "function"))
