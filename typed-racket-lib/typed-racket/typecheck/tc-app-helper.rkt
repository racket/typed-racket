#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/sequence racket/set racket/list
         (contract-req)
         (typecheck check-below tc-subst tc-metafunctions possible-domains)
         (utils tc-utils)
         (rep type-rep prop-rep values-rep)
         (except-in (types utils abbrev subtype type-table)
                    -> ->* one-of/c))
(require-for-cond-contract
  syntax/stx)

(provide/cond-contract
  [tc/funapp1
    ((syntax? stx-list? Arrow? (listof tc-results/c) (or/c #f tc-results/c))
     (#:check boolean?)
     . ->* . full-tc-results/c)])
(define (tc/funapp1 f-stx args-stx ftype0 argtys expected #:check [check? #t])
  ;; update tooltip-table with inferred function type
  (add-typeof-expr f-stx (ret (make-Fun (list ftype0))))
  (match* (ftype0 argtys)
    ;; we check that all kw args are optional
    [((Arrow: dom rest (list (Keyword: _ _ #f) ...) rng)
      (list (tc-result1: t-a _ o-a) ...))
     #:when (not (RestDots? rest))
     
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
       (for ([dom-t (if rest (in-list/rest dom rest) (in-list dom))]
             [a (in-syntax args-stx)]
             [arg-t (in-list t-a)])
         (parameterize ([current-orig-stx a]) (check-below arg-t dom-t))))
     (let ([dom-count (length dom)])
       ;; Currently do nothing with rest args and keyword args
       ;; as there are no support for them in objects yet.
       (let-values
           ([(o-a t-a) (if (= dom-count (length t-a))
                           (values o-a t-a)
                           (for/lists (os ts)
                             ([_ (in-range dom-count)]
                              [oa (in-list/rest o-a -empty-obj)]
                              [ta (in-list/rest t-a Univ)])
                             (values oa ta)))])
         (values->tc-results rng o-a t-a)))]
    ;; this case should only match if the function type has mandatory keywords
    ;; but no keywords were provided in the application
    [((Arrow: _ _ kws _) _)
     #:when (ormap Keyword-required? kws)
     (when check?
       (tc-error/fields "could not apply function"
                        #:more "a required keyword was not supplied"
                        "missing keyword"
                        (car (filter Keyword-required? kws))))]
    [((Arrow: _ (? RestDots? drest) '() _) _)
     (int-err "funapp with drest args ~a ~a NYI" drest argtys)]
    [((Arrow: _ _ kws _) _)
     (int-err "funapp with keyword args ~a NYI" kws)]))


(define (make-printable t)
  (match t
    [(tc-result1: t) (cleanup-type t)]
    [(tc-results: tcrs _)
     (-values (for/list ([tcr (in-list tcrs)])
                (cleanup-type (tc-result-t tcr))))]
    [(tc-any-results: _) (-AnyValues -tt)]
    [_ t]))

(define (stringify-domain dom rst [rng #f])
  (let ([doms-string (if (null? dom) "" (string-append (stringify (map make-printable dom)) " "))]
        [rng-string (if rng (format " -> ~a" rng) "")])
    (match rst
      [(RestDots: dty dbound)
       (format "~a~a ... ~a~a" doms-string dty dbound rng-string)]
      [rst
       (format "~a~a *~a" doms-string rst rng-string)]
      [else (string-append (stringify (map make-printable dom)) rng-string)])))

;; Generates error messages when operand types don't match operator domains.
(provide/cond-contract
  [domain-mismatches
   ((syntax? syntax? Type? (listof (listof Type?)) (listof (or/c #f Type? RestDots?))
     (listof SomeValues?) (listof tc-results?) (or/c #f Type?) any/c)
    (#:expected (or/c #f tc-results/c)
     #:return tc-results?
     #:msg-thunk (-> string? string?)
     #:arg-names (listof identifier?))
    . ->* . tc-results/c)])
(define (domain-mismatches f-stx args-stx ty doms rests rngs arg-tys tail-ty tail-bound
                           #:expected [expected #f]
                           #:return [return (ret -Bottom)]
                           #:msg-thunk [msg-thunk (lambda (dom) dom)]
                           ;; if it's a dependent function, pass the argument identifiers so we
                           ;; can report those in the error message
                           #:arg-names [arg-names '()])
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
    [(and (= 1 (length doms))
          (not (car rests))
          (not tail-ty)
          (not tail-bound))
     (tc-error/expr
      #:return return
      (msg-thunk
       (apply string-append
              (if (not (= (length (car doms)) (length arg-tys)))
                  (format "Wrong number of arguments - Expected ~a, but got ~a\n\n" (length (car doms)) (length arg-tys))
                  "")
              (append
               (for/list ([i (in-naturals 1)]
                          [dom-t (in-list (list-extend arg-tys (car doms) #f))]
                          [arg-t (in-list (list-extend (car doms) arg-tys #f))]
                          [arg-id (in-list/rest arg-names #f)])
                         (let ([dom-t (or dom-t "-none-")]
                               [arg-t (or arg-t "-none-")])
                           (cond
                             [arg-id
                              (format "Argument ~a (position ~a):\n  Expected: ~a\n  Given:    ~a\n"
                                      (syntax-e arg-id)
                                      i
                                      (make-printable dom-t)
                                      (make-printable arg-t))]
                             [else
                              (format "Argument ~a:\n  Expected: ~a\n  Given:    ~a\n"
                                      i
                                      (make-printable dom-t)
                                      (make-printable arg-t))])))
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
        (stringify-domain (car doms) (car rests))
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
     (match-let ([(list pdoms prngs prests) (possible-domains doms rests rngs expected)])
       ;; if we somehow eliminate all the cases (bogus expected type) fall back to showing the
       ;; extra cases
       (let-values ([(pdoms rngs rests)
                     (if (null? pdoms)
                         (values doms rngs rests)
                         (values pdoms prngs prests))])
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
                         (subval (car rngs) (tc-results->values expected))))
                ;; if we narrowed down the possible cases to a single one, have
                ;; tc/funapp1 generate a better error message
                (tc/funapp1 f-stx args-stx
                            (make-Arrow (car pdoms)
                                        (car rests)
                                        null
                                        (car rngs))
                            arg-tys expected)
                return]
               [else
                ;; if not, print the message as usual
                (define err-doms
                  (string-append
                   label
                   (stringify (if expected
                                  (map stringify-domain pdoms rests rngs)
                                  (map stringify-domain pdoms rests))
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
  [poly-fail ((syntax? syntax? Type? (listof tc-results?))
              (#:name (or/c #f syntax?)
               #:expected (or/c #f tc-results/c))
              . ->* . tc-results/c)])
(define (poly-fail f-stx args-stx t argtypes #:name [name #f] #:expected [expected #f])
  (match t
    [(or (Poly-names:
          msg-vars
          (Fun: (list (Arrow: msg-doms
                              msg-rests
                              (list (Keyword: _ _ #f) ...)
                              msg-rngs)
                      ...)))
         (PolyDots-names:
          msg-vars
          (Fun: (list (Arrow: msg-doms
                              msg-rests
                              (list (Keyword: _ _ #f) ...)
                              msg-rngs)
                      ...)))
         (PolyRow-names:
          msg-vars _
          (Fun: (list (Arrow: msg-doms
                              msg-rests
                              (list (Keyword: _ _ #f) ...)
                              msg-rngs)
                      ...))))
     (let ([fcn-string (name->function-str name)])
       (if (and (andmap null? msg-doms)
                (null? argtypes))
           (tc-error/expr (string-append
                           "Could not infer types for applying polymorphic "
                           fcn-string
                           "\n"))
           (domain-mismatches f-stx args-stx t msg-doms msg-rests
                              msg-rngs argtypes #f #f #:expected expected
                              #:msg-thunk (lambda (dom)
                                            (string-append
                                             "Polymorphic " fcn-string " could not be applied to arguments:\n"
                                             dom
                                             (if (not (subset? (apply set-union (seteq) (map fv/list msg-doms))
                                                               (list->seteq msg-vars)))
                                                 (string-append "Type Variables: " (stringify msg-vars) "\n")
                                                 ""))))))]
    [(Poly-names:
      msg-vars
      (DepFun/pretty-ids: ids domain _ rng))
     (let ([fcn-string (name->function-str name)])
       (if (and (null? domain)
                (null? argtypes))
           (tc-error/expr (string-append
                           "Could not infer types for applying polymorphic "
                           fcn-string
                           "\n"))
           (domain-mismatches f-stx args-stx t (list domain) (list #f)
                              (list rng) argtypes #f #f #:expected expected
                              #:msg-thunk (lambda (dom)
                                            (string-append
                                             "Polymorphic " fcn-string " could not be applied to arguments:\n"
                                             dom
                                             (if (not (subset? (fv/list domain) (list->seteq msg-vars)))
                                                 (string-append "Type Variables: " (stringify msg-vars) "\n")
                                                 "")))
                              #:arg-names ids)))]
    [(or (Poly-names:
          msg-vars
          (Fun: (list (Arrow: msg-doms msg-rests kws msg-rngs) ...)))
         (PolyDots-names:
          msg-vars
          (Fun: (list (Arrow: msg-doms msg-rests kws msg-rngs) ...)))
         (PolyRow-names:
          msg-vars
          _
          (Fun: (list (Arrow: msg-doms msg-rests kws msg-rngs) ...))))
     (let ([fcn-string (if name
                           (format "function with keywords ~a" (syntax->datum name))
                           "function with keywords")])
       (if (and (andmap null? msg-doms)
                (null? argtypes))
           (tc-error/expr (string-append
                           "Could not infer types for applying polymorphic "
                           fcn-string
                           "\n"))
           (domain-mismatches f-stx args-stx t msg-doms msg-rests
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

