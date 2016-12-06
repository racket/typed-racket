#lang racket/unit

(require "../utils/utils.rkt"
         syntax/parse
         racket/match
         "signatures.rkt" "tc-metafunctions.rkt"
         "tc-funapp.rkt"
         (types utils abbrev resolve subtype match-expanders)
         (typecheck check-below)
         (private syntax-properties)
         (utils tc-utils)
         (for-syntax racket/base syntax/parse)
         (for-template racket/base)
         (rep type-rep prop-rep object-rep))

(import tc-if^ tc-lambda^ tc-app^ tc-let^ tc-expr^)
(export check-subforms^)

;; Does a depth first search of the syntax object. For each sub object it attempts to match it
;; against the provide syntax-parse patterns.
(define-syntax find-syntax
  (syntax-parser
    [(_ init-form [clause bodies ...+] ...)
     #'(let loop ([form init-form])
         (parameterize ([current-orig-stx form])
           (syntax-parse form
             #:literals (quote-syntax)
             [clause bodies ...] ...
             ;; avoid going under quote-syntax, nothing to typecheck
             [(quote-syntax . rst) (void)]
             [(a . b)
              (loop #'a)
              (loop #'b) ]
             [_ (void)])))]))

;; find the subexpressions that need to be typechecked in an ignored form
;; syntax (or/c #f tc-results/c) -> full-tc-results/c
(define (check-subforms/with-handlers form expected)
  (define predicate-map (make-hash))
  (define handler-map (make-hash))
  (define body-results #f)

  ;; syntax tc-result1 type -> tc-results
  ;; The result of applying the function to a single argument of the type of its first argument.
  ;; Is used when checking forms like with-handlers, for example.
  (define (get-range-result stx t prop-type)
    (let loop ((t t))
      (match t
        [(Function: (list _ ... (arr: (list arg1) _ _ #f (list (Keyword: _ _ #f) ...)) _ ...))
         #:when (subtype prop-type arg1)
         (tc/funapp #'here #'(here) t (list (ret arg1)) #f)]
        [(Function: (list _ ... (arr: '() _ (? values rest) #f (list (Keyword: _ _ #f) ...)) _ ...))
         #:when (subtype prop-type rest)
         (tc/funapp #'here #'(here) t (list (ret rest)) #f)]
        [(? resolvable? t)
         (loop (resolve t))]
        [(or (Poly: ns _) (PolyDots: (list ns ... _) _))
         (loop (instantiate-poly t (map (λ (n) Univ) ns)))]
        ;; This clause should raise an error via the check-below test
        [_
         (cond [;; a redundant test, but it ensures an error message below
                (not (subtype t (-> prop-type Univ)))
                (parameterize ([current-orig-stx stx])
                  (check-below t (-> prop-type Univ)))]
               [else (int-err "get-range-result: should not happen. type ~a prop ~a"
                              t prop-type)])
         (ret (Un))])))

  ;; Syntax Type -> (Option Type)
  ;; Extract the type for the prop in a predicate type, or #f if
  ;; the type is an invalid predicate type.
  (define (get-prop-type stx pred-type)
    (cond [;; make sure the predicate has an appropriate type
           (subtype pred-type (-> Univ Univ))
           (define fun-type
             (if (resolvable? pred-type)
                 (resolve pred-type)
                 pred-type))
           (match fun-type
             ;; FIXME: Almost all predicates fall into this case, but it may
             ;;        be worth being more precise here for some rare code.
             [(PredicateProp: ps)
              (match ps
                [(PropSet: (TypeProp: (Path: '() (cons 0 0)) ft) _) ft]
                [(FalseProp:) (Un)]
                [_ Univ])]
             [_ Univ])]
          [else
           ;; if the type is wrong, produce a nice error message
           (parameterize ([current-orig-stx stx])
             (check-below pred-type (-> Univ Univ)))
           #f]))

  ;; -> (Listof Type)
  ;; Produce a list of result types from the predicate/handler maps
  (define (get-handler-results)
    (for/list ([key (in-hash-keys predicate-map)])
      (match-define (list predicate-stx predicate-type)
        (hash-ref predicate-map key))
      (match-define (list handler-stx handler-type)
        (hash-ref handler-map key))
      (define prop-type
        (get-prop-type predicate-stx predicate-type))
      ;; if the predicate doesn't check, then don't bother
      ;; with the RHS and return no result
      (if prop-type
          (get-range-result handler-stx handler-type prop-type)
          (ret (Un)))))

  (find-syntax form
    ;; if this needs to be checked
    [stx:with-type^
     ;; the form should be already ascribed the relevant type
     (tc-expr #'stx)]
    ;; exception predicate
    [stx:exn-predicate^
     (match (single-value #'stx)
       [(tc-result1: t)
        (hash-set! predicate-map (attribute stx.value) (list #'stx t))])]
    ;; this is a handler function
    [stx:exn-handler^
     (match (single-value #'stx)
       [(tc-result1: t)
        (hash-set! handler-map (attribute stx.value) (list #'stx t))])]
    ;; this is the body of the with-handlers
    [stx:exn-body^
     (set! body-results (tc-expr/check #'stx expected))])
  (define handler-results (get-handler-results))
  (merge-tc-results (cons body-results handler-results)))

;; typecheck the expansion of a with-handlers form
;; syntax -> void
(define (check-subforms/ignore form)
  (find-syntax form
    ;; if this needs to be checked
    [stx:with-type^
     ;; the form should be already ascribed the relevant type
     (void (tc-expr #'stx))]))
