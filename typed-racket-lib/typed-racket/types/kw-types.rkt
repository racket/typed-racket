#lang racket/base

(require "../utils/utils.rkt"
         (utils tc-utils)
         (types abbrev tc-result utils)
         (rep core-rep type-rep values-rep)
         (base-env annotate-classes)
         racket/list racket/set racket/match
         racket/format racket/string
         syntax/parse)


(define (convert keywords ; (listof Keyword?)
                 kw-opts-supplied ; (listof Keyword?)
                 mandatory-arg-types ; (listof Type?)
                 optional-arg-types ; (listof Type?)
                 pos-opts-supplied? ; (or/c (listof Boolean?) #f)
                 rng ; SomeValues?
                 maybe-rst ; (or/c #f Type? RestDots?)
                 split?) ; boolean?
  (define rst-type (match maybe-rst
                     [#f '()]
                     [(? Rest?) (list (Rest->Type maybe-rst))]
                     [_ (int-err "Invalid rest kind passed to kw-convert")]))

  ;; the kw protocol puts the arguments in keyword-sorted order in the
  ;; function header, so we need to sort the types to match
  (define sorted-kws
    (sort keywords (λ (kw1 kw2) (keyword<? (Keyword-kw kw1)
                                           (Keyword-kw kw2)))))

  (define pos-opt-arg-types
    (append (for/list ([t (in-list optional-arg-types)]
                       [supplied? (in-list pos-opts-supplied?)])
              (if supplied?
                  t
                  (Un t -Unsafe-Undefined)))
            (let ([num-opts (length optional-arg-types)])
              (make-missing-opt-args (- (length pos-opts-supplied?) num-opts)
                                     (list-tail pos-opts-supplied? num-opts)))))

  (make-Fun
   (cond
     [(not split?)
      (define ts
        (flatten
         (list
          (for/list ([k (in-list sorted-kws)])
            (match k
              [(Keyword: _ t #t) t]
              [(Keyword: kw t #f) (if (member kw kw-opts-supplied)
                                      t
                                      (Un t -Unsafe-Undefined))]))
          mandatory-arg-types
          pos-opt-arg-types
          rst-type)))

      (list (-Arrow ts rng))]
     [else
      ;; The keyword argument types, including undefined option for
      ;; optional keyword arguments
      (define kw-args
        (for/fold ([pos '()])
                  ([k (in-list (reverse sorted-kws))])
          (match k
            ;; mandatory keyword arguments have no extra type option
            [(Keyword: _ t #t) (cons t pos)]
            ;; optional keyword arguments might be always supplied
            ;; in the expansion
            [(Keyword: kw t #f)
             (if (member kw kw-opts-supplied)
                 (cons t pos) ; keyword always supplied by expansion, so no unsafe-undefined check
                 (cons (Un t -Unsafe-Undefined) pos))])))

      (list (-Arrow (append kw-args mandatory-arg-types pos-opt-arg-types rst-type)
                    rng))])))

;; This is used to fix the props of keyword types.
;; TODO: This should also explore deeper into the actual types and remove props in there as well.
;; TODO: This should not remove the props but instead make them refer to the actual arguments after
;; keyword conversion.
(define (erase-props/Values values)
  (match values
    [(AnyValues: _) ManyUniv]
    [(Values: rs)
     (make-Values
      (map (match-lambda
             [(Result: t _ _) (-result t)])
           rs))]
    [(ValuesDots: rs dty dbound)
     (make-ValuesDots (make-Values
                       (map (match-lambda
                              [(Result: t _ _) (-result t)])
                            rs))
                      dty
                      dbound)]))

;; handle-extra-or-missing-kws : (Listof Keyword) LambdaKeywords
;;                               -> (Listof Keyword)
;; Remove keywords in the given list that aren't in the actual lambda
;; keywords list. This allows the typechecker to check some branches of the
;; type that match the actual kws. Add extra actual keywords with Bottom type.
(define (handle-extra-or-missing-kws kws actual-kws)
  (match-define (lambda-kws actual-mands actual-opts actual-opts-supplied
                            actual-pos-mand-count actual-pos-opts-supplied?)
    actual-kws)
  (define expected-kws (map Keyword-kw kws))
  (define missing-removed
    (filter
     (λ (kw) (or (member (Keyword-kw kw) actual-mands)
                 (member (Keyword-kw kw) actual-opts)))
     kws))
  (append missing-removed
          (for/list ([kw (in-list (set-subtract actual-mands expected-kws))])
            (make-Keyword kw -Bottom #t))
          (for/list ([kw (in-list (set-subtract actual-opts expected-kws))])
            (make-Keyword kw -Bottom #f))))

;; is arrow's max arity one less than arrow*'s min arity,
;; and arrow cannot have infinite max arity,
;; and both arrows have equivalent keyword specs
(define (domain-is-prefix? arrow arrow* min-arity)
  (and (not (Arrow-rst arrow))
       (equal? (Arrow-kws arrow) (Arrow-kws arrow*))
       (= (Arrow-max-arity arrow) (sub1 min-arity))
       (for/and ([d (in-list (Arrow-dom arrow))]
                 [d* (in-list (Arrow-dom arrow*))])
         (equal? d d*))))

;; calculate-mandatory-args
;;   : (listof Arrow?) -> (assoc-listof Arrow? exact-nonnegative-integer?)
;; We don't explicitly record optional vs mandatory args on arrows.
;; This function looks for Arrows which, in a case->, are equivalent
;; to having optional arguments and returns a dictionary mapping
;; arrows to their mandatory arg count (from which we can derive their
;; optional arg count)
;; e.g. (calculate-mandatory-args (list (-> String Any) (-> String String Any)))
;;   ==> (list (cons (-> String String Any) 1))
;; meaning instead of remembering both arrows, we can just remember
;; (-> String String Any) and the fact that only the first positional argument
;; is mandatory (i.e. the second is optional)
(define (calculate-mandatory-args orig-arrows)
  ;; sorted order is important, our loops below rely on this order
  (define arity-sorted-arrows
    (sort orig-arrows
          (λ (a1 a2) (>= (Arrow-max-arity a1)
                         (Arrow-max-arity a2)))))
  (for/fold ([mand-arg-table '()])
            ([arrow (in-list arity-sorted-arrows)])
    (cond
      [(for/or ([(arrow* min-arity) (in-assoc mand-arg-table)])
         ;; is arrow like arrow* but arrow's max arity is 1 less
         ;; than arrow*'s currently known min arity?
         (and (domain-is-prefix? arrow arrow* min-arity)
              (assoc-set mand-arg-table arrow* (sub1 min-arity))))]
      [else
       (assoc-set mand-arg-table arrow (Arrow-min-arity arrow))])))

;; inner-kw-convert : (Listof arr) (Option LambdaKeywords) Boolean -> Type
;; Helper function that converts each arr to a Function type and then
;; merges them into a single Function type.
(define (inner-kw-convert arrs actual-kws split?)
  (define mand-arg-table (calculate-mandatory-args arrs))
  (define fns
    ;; use for/list and remove duplicates afterwards instead of
    ;; set and set->list to retain determinism
    (remove-duplicates
     (for/list ([(arrow arrow-mand-arg-count) (in-assoc mand-arg-table)])
       (match arrow
         [(Arrow: dom rst kws rng)
          (define kws* (if actual-kws
                           (handle-extra-or-missing-kws kws actual-kws)
                           kws))
          (define kw-opts-supplied (if actual-kws
                                       (lambda-kws-opt-supplied actual-kws)
                                       '()))
          (define mand-arg-count (if actual-kws
                                     (lambda-kws-pos-mand-count actual-kws)
                                     arrow-mand-arg-count))
          (define opt-arg-count (- (length dom) mand-arg-count))
          (define extra-opt-arg-count
            ;; In case `dom` has too many arguments that we try to treat
            ;; as optional:
            (if actual-kws
                (max 0 (- opt-arg-count (length (lambda-kws-pos-opt-supplied? actual-kws))))
                0))
          (convert kws*
                   kw-opts-supplied
                   (take dom mand-arg-count)
                   (drop dom mand-arg-count)
                   (if actual-kws
                       (append (lambda-kws-pos-opt-supplied? actual-kws)
                               (make-list extra-opt-arg-count #f))
                       (make-list opt-arg-count #f))
                   rng
                   rst
                   split?)]))))
  (apply cl->* fns))

;; kw-convert : Type (Option LambdaKeywords) [Boolean] -> Type
;; Given an ordinary function type, convert it to a type that matches the keyword
;; function protocol
(define (kw-convert ft actual-kws #:split [split? #f])
  (match ft
    [(Fun: arrs)
     (inner-kw-convert arrs actual-kws split?)]
    [(Poly-names: names f)
     (make-Poly names (kw-convert f actual-kws #:split split?))]
    [(PolyDots-names: names f)
     (make-PolyDots names (kw-convert f actual-kws #:split split?))]))

;; kw-unconvert : Type (Listof Syntax) (Listof Keyword) (Listof Keyword) -> Type
;; Given a type for a core keyword function, unconvert it to a
;; normal function type.
;;
;; precondition: only call this for functions with no type annotations,
;;               which means they will never have polymorphic types.
(define (kw-unconvert ft formalss mand-keywords keywords)
  (define (lengthish formals)
    (define lst (syntax->list formals))
    (if lst (length lst) +inf.0))
  ;; only need longest formals and smallest formals to determine
  ;; which arguments are optional non-kw arguments
  (define max-formals (argmax lengthish formalss))
  (define min-formals (argmin lengthish formalss))
  (define-values (raw-non-kw-argc rest?)
    (syntax-parse max-formals
      [(_ _ non-kw:id ...)
       (values (length (syntax->list #'(non-kw ...))) #f)]
      [(_ _ non-kw:id ... . rst:id)
       (values (length (syntax->list #'(non-kw ...))) #t)]))
  (define opt-non-kw-argc
    (syntax-parse min-formals
      [(_ _ non-kw:id ...)
       (- raw-non-kw-argc (length (syntax->list #'(non-kw ...))))]
      ;; if min and max both have rest args, then there cannot
      ;; have been any optional arguments
      [(_ _ non-kw:id ... . rst:id) 0]))
  (define non-kw-argc raw-non-kw-argc)
  (define mand-non-kw-argc (- non-kw-argc opt-non-kw-argc))
  (match ft
    [(Fun: arrs)
     (cond [(positive? (length arrs))
            ;; assumption: only one arr is needed, since the types for
            ;; the actual domain are the same
            (match-define (Arrow: doms _ _ rng) (car arrs))
            (define kw-length
              (- (length doms) (+ non-kw-argc (if rest? 1 0))))
            (define kw-args (take doms kw-length))
            (define actual-kws (compute-kws mand-keywords keywords kw-args))
            (define other-args (drop doms kw-length))
            (define-values (mand-args opt-and-rest-args)
              (split-at other-args mand-non-kw-argc))
            (define rest-type
              (and rest? (last opt-and-rest-args)))
            (define opt-types (take opt-and-rest-args opt-non-kw-argc))
            (define opt-types-count (length opt-types))
            (make-Fun
             (for/list ([to-take (in-range (add1 opt-types-count))])
               (-Arrow (append mand-args
                               (take opt-types to-take))
                       (erase-props/Values rng)
                       #:kws actual-kws
                       #:rest (if (= to-take opt-types-count) rest-type #f))))]
           [else (int-err "unsupported arrs in keyword function type")])]
    [_ (int-err "unsupported keyword function type")]))

;; check-kw-arity : LambdaKeywords Type -> Void
;;
;; Check if the TR lambda property listing the keywords matches up with
;; the type that we've given. Allows for better error messages than just
;; relying on tc-expr. Return #f if the function shouldn't be checked.
(define (check-kw-arity kw-prop f-type)
  (match-define (lambda-kws actual-mands actual-opts actual-opts-supplied
                            actual-pos-mand-count actual-pos-opts-supplied?)
    kw-prop)
  (define arrs
    (match f-type
      [(AnyPoly-names: _ _ (Fun: arrs)) arrs]))
  (for*/and ([arr (in-list arrs)]
             [kws (in-value (Arrow-kws arr))])
    (define-values (mand-kw-types opt-kw-types) (partition-kws kws))
    (define mand-kws (map Keyword-kw mand-kw-types))
    (define opt-kws (map Keyword-kw opt-kw-types))
    (define missing-opts (set-subtract opt-kws actual-opts))
    (define missing-mands (set-subtract mand-kws actual-mands))
    ;; extra optional keywords are okay
    (define extra-kws (set-subtract actual-mands mand-kws))
    (unless (and (set-empty? missing-mands)
                 (set-empty? missing-opts))
      (tc-error/fields
       #:delayed? #t
       "type mismatch"
       #:more "function is missing keyword arguments"
       "missing mandatory keywords"
       (string-join (map ~a missing-mands))
       "missing optional keywords"
       (string-join (map ~a missing-opts))
       "expected type" f-type))
    (unless (set-empty? extra-kws)
      (tc-error/fields
       #:delayed? #t
       "type mismatch"
       #:more "function has too many mandatory keyword arguments"
       "extra keywords"
       (string-join (map ~a extra-kws))
       "expected type" f-type))))

;; compute-kws : (Listof Keyword) (Listof Keyword) (Listof Type)
;;               -> (Listof make-Keyword)
;; Computes the keyword types for an arr in kw-unconvert
;;
;; assumptions: (1) in kw-args, there are two types per optional kw
;;                  and the first type is the argument type (which is
;;                  the same in every `arr` in the function type)
;;              (2) kw-args and keywords are sorted by keyword<? order
(define (compute-kws mand-keywords keywords kw-args)
  (let loop ([kw-args kw-args]
             [keywords keywords]
             [kw-types '()])
    (cond [(empty? kw-args) (reverse kw-types)]
          [(memq (car keywords) mand-keywords)
           (loop (cdr kw-args) (cdr keywords)
                 (cons (make-Keyword (car keywords) (car kw-args) #t)
                       kw-types))]
          [else ; optional
           (loop (cdr kw-args) (cdr keywords)
                 (cons (make-Keyword (car keywords) (car kw-args) #f)
                       kw-types))])))

(define (opt-convert-arr required-pos optional-pos optional-supplied? arr)
  (match arr
    [(Arrow: args #f '() result)
     (define num-args (length args))
     (and (>= num-args required-pos)
          (<= num-args (+ required-pos optional-pos))
          (let* ([required-args (take args required-pos)]
                 [opt-args (for/list ([arg (in-list (drop args required-pos))]
                                      [supplied? (in-list optional-supplied?)])
                             (if supplied?
                                 arg
                                 (Un -Unsafe-Undefined arg)))])
            (-Arrow (append required-args
                            opt-args
                            (make-missing-opt-args (- (+ required-pos optional-pos) num-args)
                                                   (list-tail optional-supplied? (- num-args required-pos))))
                    result)))]
    [_ #f]))

(define (make-missing-opt-args num-missing-opt-args supplied?s)
  (for/list ([i (in-range num-missing-opt-args)]
             [supplied? (in-list supplied?s)])
    (if supplied?
        ;; body will get the right type from other `if` branch:
        (Un)
        ;; body can deal with an unsafe-undefined argument:
        -Unsafe-Undefined)))

(define (opt-convert ft required-pos optional-pos optional-supplied?)
  (let loop ([ft ft])
    (match ft
      [(Fun: arrs)
       ;; We expect only one of `arrs` to have all optional arguments, but
       ;; accomodate multiple of them
       (let ([arrs (for*/list ([arr (in-list arrs)]
                               [new-arr (in-value
                                         (opt-convert-arr required-pos optional-pos optional-supplied? arr))]
                               #:when new-arr)
                     new-arr)])
         (and (pair? arrs)
              (make-Fun arrs)))]
      [(Poly-names: names f)
       (match (loop f)
         [#f #f]
         [t (make-Poly names t)])]
      [(PolyDots-names: names f)
       (match (loop f)
         [#f #f]
         [t (make-PolyDots names t)])]
      [t t])))

;; opt-unconvert : Type (Listof Syntax) -> Type
;; Given a type for a core optional arg function, unconvert it to a
;; normal function type. See `kw-unconvert` above.
(define (opt-unconvert ft formalss)
  (define (lengthish formals)
    (define lst (syntax->list formals))
    (if lst (length lst) +inf.0))
  (define max-formals (argmax lengthish formalss))
  (define min-formals (argmin lengthish formalss))
  (define-values (raw-argc rest?)
    (syntax-parse max-formals
      [(arg:id ...)
       (values (length (syntax->list #'(arg ...))) #f)]
      [(arg:id ... . rst:id)
       (values (length (syntax->list #'(arg ...))) #t)]))
  (define opt-argc
    (syntax-parse min-formals
      [(arg:id ...)
       (- raw-argc (length (syntax->list #'(arg ...))))]
      ;; if min and max both have rest args, then there cannot
      ;; have been any optional arguments
      [(arg:id ... . rst:id) 0]))
  (define mand-argc (- raw-argc opt-argc))
  (match ft
    [(Fun: arrs)
     (cond [(= 1 (length arrs))
            (match-define (Arrow: doms _ _ rng) (car arrs))
            (define-values (mand-args opt-and-rest-args)
              (split-at doms mand-argc))
            (define rest-type
              (and rest? (last opt-and-rest-args)))
            (define opt-types (take opt-and-rest-args opt-argc))
            (define opt-types-len (length opt-types))
            (make-Fun
             (for/list ([how-many-opt-args (in-range (add1 opt-types-len))])
               (-Arrow (append mand-args (take opt-types how-many-opt-args))
                       rng
                       #:rest (and (= how-many-opt-args opt-types-len)
                                   rest-type))))]
           [else (int-err "unsupported arrs in keyword function type")])]
    [_ (int-err "unsupported keyword function type")]))

;; partition-kws : (Listof Keyword) -> (values (Listof Keyword) (Listof Keyword))
;; Partition keywords by whether they are mandatory or not
(define (partition-kws kws)
  (partition (match-lambda [(Keyword: _ _ mand?) mand?]) kws))

(provide kw-convert kw-unconvert opt-convert opt-unconvert partition-kws
         check-kw-arity)
