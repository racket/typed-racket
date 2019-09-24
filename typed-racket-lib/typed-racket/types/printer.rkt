#lang racket/base

;; This module provides functions for printing types and related
;; data structures such as propositions and objects

(require racket/require racket/match racket/dict racket/string racket/promise
         racket/pretty
         racket/list
         racket/set
         syntax/id-set
         (except-in (path-up "utils/utils.rkt") infer)
         (path-up "rep/type-rep.rkt" "rep/prop-rep.rkt" "rep/object-rep.rkt"
                  "rep/core-rep.rkt" "rep/values-rep.rkt" "rep/fme-utils.rkt"
                  "rep/rep-utils.rkt" "rep/free-ids.rkt"
                  "types/subtype.rkt" "types/overlap.rkt"
                  "types/match-expanders.rkt"
                  "types/kw-types.rkt"
                  "types/utils.rkt" "types/abbrev.rkt"
                  "types/union.rkt" "types/numeric-tower.rkt"
                  "types/resolve.rkt"
                  "utils/prefab.rkt" "utils/identifier.rkt"
                  "utils/tc-utils.rkt"
                  "types/struct-table.rkt"
                  "infer/infer.rkt"
                  "types/substitute.rkt")
         (for-syntax racket/base syntax/parse))

;; printer-type: (one-of/c 'custom 'debug)
(define-for-syntax printer-type 'custom)
(define-syntax (provide-printer stx)
  (if (eq? printer-type 'debug)
      #'(provide (rename-out [debug-printer print-type]
                             [debug-printer print-prop]
                             [debug-printer print-propset]
                             [debug-printer print-values]
                             [debug-printer print-result]
                             [debug-printer print-object]
                             [debug-printer print-pathelem]
                             [debug-pretty-format-type pretty-format-rep]))
      #'(provide print-type print-prop print-propset print-object print-pathelem
                 pretty-format-rep print-values print-result)))
(provide-printer)

(provide print-complex-props? type-output-sexpr-tweaker
         current-print-type-fuel current-print-unexpanded)


;; do we attempt to find instantiations of polymorphic types to print?
;; FIXME - currently broken
(define print-poly-types? #t)
;; do we use simple type aliases in printing
(define print-aliases #t)

(define type-output-sexpr-tweaker (make-parameter values))
(define print-complex-props? (make-parameter #f))

;; this parameter controls how far down the type to expand type names
;; interp. 0 -> don't expand
;;         1 -> expand one level, etc.
;;    +inf.0 -> expand always
(define current-print-type-fuel (make-parameter 0))

;; this parameter allows the printer to communicate unexpanded
;; type aliases to its clients, which is used to cue the user
(define current-print-unexpanded (make-parameter (box '())))

;; does t have a type name associated with it currently?
;; has-name : Type -> Maybe[Listof<Symbol>]
(define (has-name? t)
  (define candidates
    (for/list ([(n t*) (in-dict (force (current-type-names)))]
               #:when (and print-aliases (Type? t*) (equal? t t*)))
      n))
  (and (pair? candidates)
       (sort candidates string>? #:key symbol->string #:cache-keys? #t)))

;; primitive<=?
;;
;; provides a consistent ordering when printing things like
;; unions, intersections, etc
(define (primitive<=? s1 s2)
  (string<=? (format "~a" s1)
             (format "~a" s2)))

;; print-<thing> : <thing> Output-Port Boolean -> Void
;; print-type also takes an optional (Listof Symbol)
;;
;; These four functions call the helpers below to print an
;; s-expression representation of the given type/pathelem/prop/object.
(define (print-type type port write? [ignored-names '()])
  (display (type->sexp type ignored-names) port))


(define (print-prop prop port write?)
  (display (prop->sexp prop) port))


(define (print-propset prop port write?)
  (display (propset->sexp prop) port))

(define (print-object obj port write?)
  (display (object->sexp obj) port))

(define (print-pathelem pe port write?)
  (display (cons 'PathElem (vector->list (struct->vector pe))) port))

(define (print-result res port write?)
  (display (result->sexp res) port))

(define (print-values vals port write?)
  (display (values->sexp vals) port))

;; Table for formatting pretty-printed types
(define type-style-table
  (pretty-print-extend-style-table
   #f '(U All -> ->*) '(and lambda and and)))

;; pretty-format-type : Type -> String
;; Formats the type using pretty printing
(define (pretty-format-rep rep #:indent [indent 0])
  (define out (open-output-string))
  (port-count-lines! out)
  (write-string (make-string indent #\space) out)
  (parameterize ([pretty-print-current-style-table type-style-table])
    (pretty-display ((type-output-sexpr-tweaker) (match rep
                                                   [(? Type?) (type->sexp rep '())]
                                                   [(? SomeValues?) (values->sexp rep)]
                                                   [(? Result?) (result->sexp rep)]))
                    out))
  (string-trim #:left? #f (substring (get-output-string out) indent)))

(define name-ref->sexp
  (match-lambda
    [(? syntax? name-ref) (syntax-e name-ref)]
    [(cons lvl arg) `(,lvl ,arg)]))

;; prop->sexp : Prop -> S-expression
;; Print a Prop (see prop-rep.rkt) to the given port
(define (prop->sexp prop)
  (match prop
    [(NotTypeProp: o type)
     `(! ,(object->sexp o) ,(type->sexp type))]
    [(TypeProp: o type)
     `(: ,(object->sexp o) ,(type->sexp type))]
    [(TrueProp:) 'Top]
    [(FalseProp:) 'Bot]
    [(AndProp: ps)
     ;; We do a little bit of work here to print equalities
     ;; instead of (<= x y) (<= y x) when we have both inequalities
     (define-values (leqs others) (partition LeqProp? ps))
     (define-values (eqs simple-leqs)
       (for/fold ([eqs '()] [simple-leqs '()])
                 ([leq (in-list leqs)])
         (match leq
           [(LeqProp: lhs rhs)
            (define flip (-leq rhs lhs))
            (cond
              [(not (member flip leqs))
               (values eqs (cons leq simple-leqs))]
              [(member flip eqs) (values eqs simple-leqs)]
              [else (values (cons leq eqs) simple-leqs)])])))
     (let ([simple-leqs (map prop->sexp simple-leqs)]
           [eqs (for/list ([leq (in-list eqs)])
                  (match leq
                    [(LeqProp: lhs rhs) `(= ,(object->sexp lhs) ,(object->sexp rhs))]))]
           [others (map prop->sexp others)])
       (match (append eqs simple-leqs others)
         [(list sexp) sexp]
         [sexps `(and ,@sexps)]))]
    [(OrProp: ps) `(or ,@(map prop->sexp ps))]
    [(LeqProp: (and lhs (LExp: 1 _))
               (and rhs (LExp: 0 _)))
     `(< ,(object->sexp (-lexp-sub1 lhs)) ,(object->sexp rhs))]
    [(LeqProp: lhs rhs) `(<= ,(object->sexp lhs) ,(object->sexp rhs))]
    [else `(Unknown Prop: ,(struct->vector prop))]))


;; c is the constant (exact-integer)
;; terms is the mapping of objects to coefficients
(define (linear-expression->sexp c terms)
  (cond
    [(terms-empty? terms) c]
    [else
     (define (positive-term? t)
       (match t
         [(? number? n) (exact-positive-integer? n)]
         [(list '* (? number? n) var) (exact-positive-integer? n)]
         [(or (? symbol?) (? list?)) ;; obj w/ coeff 1
          #t]))
     (define term-list
       (let ([terms (for/list ([(obj coeff) (in-terms terms)])
                      (if (= 1 coeff)
                          (object->sexp obj)
                          `(* ,coeff ,(object->sexp obj))))])
         (if (zero? c) terms (cons c terms))))
     (cond
       [(null? (cdr term-list)) (car term-list)]
       [else
        (define-values (pos-terms neg-terms) (partition positive-term? term-list))
        (define (flip-sign term)
          (match term
            [(? number? n) (* -1 n)]
            [(list '* (? number? n) obj)
             (if (= -1 n)
                 obj
                 `(* ,(* -1 n) ,obj))]
            [(or (? symbol? obj) (? list? obj)) ;; obj w/ coeff 1
             (list '* -1 obj)]))
        (cond
          [(null? neg-terms) (cons '+ pos-terms)]
          ;; if we have zero or one positive term t1,
          ;; and the rest (-t2 -t3 etc) are negative,
          ;; turn it into (- t1 t2 t3 ...) (where t1 may be omitted)
          [(<= (length pos-terms) 1)
           (append '(-)
                   pos-terms
                   (map flip-sign neg-terms))]
          ;; otherwise we have some negative terms (-t1 -t2 ...)
          ;; and two or more positive terms (t3 t4 ...),
          ;; convert it into (- (+ t3 t4 ...) t1 t2 ...)
          [else
           (append '(-)
                   (cons '+ pos-terms)
                   (map flip-sign neg-terms))])])]))

;; object->sexp : Object -> S-expression
;; Print an Object (see object-rep.rkt) to the given port
(define (object->sexp object)
  ;; take a `adaa` (or similar) turn it into `cadaar`
  (define (pair-seq->sym seq)
    (string->symbol (apply string-append (append (cons "c" seq) (list "r")))))
  (match object
    [(Empty:) '-]
    [(Path: pes n)
     (for/fold ([sexp (name-ref->sexp n)]
                [pair-seq '()]
                [depth 0]
                #:result (cond
                           [(not (null? pair-seq))
                            (list (pair-seq->sym pair-seq) sexp)]
                           [else sexp]))
               ([pe (in-list (reverse pes))])
       (let ([sexp (if (= 4 (length pair-seq))
                       (list (pair-seq->sym pair-seq) sexp)
                       sexp)])
         (match pe
           [(CarPE:) (values sexp (cons "a" pair-seq) (add1 depth))]
           [(CdrPE:) (values sexp (cons "d" pair-seq) (add1 depth))]
           [_
            (let ([sexp (if (not (null? pair-seq))
                            (list (pair-seq->sym pair-seq) sexp)
                            sexp)])
              (values
               (match pe
                 [(ForcePE:) (list 'force sexp)]
                 [(StructPE: t idx)
                  (define maybe-accessor-id
                    (find-struct-accessor-id
                     (λ (t* idx*) (and (subtype t t*)
                                       (= idx idx*)))))
                  (cond
                    [maybe-accessor-id
                     (list (syntax-e maybe-accessor-id) sexp)]
                    [else (list 'struct-ref sexp idx)])]
                 [(PrefabPE: key idx)
                  (define maybe-accessor-id
                    (find-struct-accessor-id
                     (λ (t* idx*) (and (Prefab? t*)
                                       (prefab-key-subtype? (Prefab-key t*) key)
                                       (= idx idx*)))))
                  (cond
                    [maybe-accessor-id
                     (list (syntax-e maybe-accessor-id) sexp)]
                    [else (list 'prefab-ref sexp idx)])]
                 [(VecLenPE:) (list 'vector-length sexp)]
                 [(SyntaxPE:) (list 'syntax-e sexp)]
                 [_ `((Invalid Path-Element: ,(struct->vector pe)) ,sexp)])
               '()
               (add1 depth)))])))]
    [(LExp: c terms) (linear-expression->sexp c terms)]
    [else `(Unknown Object: ,(struct->vector object))]))

;; cover-union : Type LSet<Type> -> Listof<Symbol> Listof<Type>
;; Unions are represented as a flat list of branches. In some cases, it would
;; be nicer to print them using higher-level descriptions instead.
;; We do set coverage, with the elements of the union being what we want to
;; cover, and all the names types we know about being the sets.
(define (cover-union t elems ignored-names)
  (define valid-names
    ;; We keep only the unions and (instantiated) polymorphic unions
    ;; which are subtypes of t. It's no use attempting to cover t with
    ;; things that go outside of t.
    (filter-map
     (match-lambda
       [(and name/type (cons name t*))
        (and
         (not (member name ignored-names))
         (match t*
           [(or (? Union?) (? BaseUnion?))
            (and (subtype t* t) name/type)]
           [(Poly: names (and raw-body (or (? Union?) (? BaseUnion?))))
            (match (infer names null (list raw-body) (list t) Univ)
              [(and (? hash? type-sub)
                    (app (λ (sub) (subst-all sub raw-body))
                         (and body (or (? Union?) (? BaseUnion?)))))
               (define args (for/list ([arg-name (in-list names)])
                              (type->sexp (t-subst-type (hash-ref type-sub arg-name)))))
               (cons (cons name args) body)]
              [_ #f])]
           [_ #f]))])
     (force (current-type-names))))
  ;; names and the sets themselves (not the union types)
  ;; note that racket/set supports lists with equal?
  (define candidates
    (map (match-lambda [(cons name (Union-all-flat: elts)) (cons name elts)]
                       [(cons name (BaseUnion-bases: elts)) (cons name elts)])
         valid-names))
  ;; some types in the union may not be coverable by the candidates
  ;; (e.g. type variables, etc.)
  (define-values (uncoverable coverable)
    (values (apply set-subtract elems (map cdr candidates))
            (set-intersect elems (apply set-union null (map cdr candidates)))))
  ;; set cover, greedy algorithm, ~lg n approximation
  (let loop ([to-cover   coverable]
             [candidates candidates]
             [coverage   '()])
    (cond [(null? to-cover) ; done
           (define coverage-names (map car coverage))
           ;; to allow :type to cue the user on unexpanded aliases
           ;; only union types can flow here, and any of those could be expanded
           (set-box! (current-print-unexpanded)
                     (append coverage-names (unbox (current-print-unexpanded))))
           ;; reverse here to retain the old ordering from when srfi/1 was
           ;; used to process the list sets
           (values coverage-names (reverse uncoverable))] ; we want the names
          [else
           ;; pick the candidate that covers the most uncovered types
           (define (covers-how-many? c)
             (length (set-intersect (cdr c) to-cover)))
           (define-values (next _)
             (for/fold ([next      (car candidates)]
                        [max-cover (covers-how-many? (car candidates))])
                 ([c (in-list candidates)])
               (let ([how-many? (covers-how-many? c)])
                 (if (> how-many? max-cover)
                     (values c how-many?)
                     (values next max-cover)))))
           (loop (set-subtract to-cover (cdr next))
                 (remove next candidates)
                 (cons next coverage))])))

;; arr->sexp : arr -> s-expression
;; Convert an arr (see type-rep.rkt) to its printable form
(define (arr->sexp arr)
  (match arr
    [(Arrow: dom rst kws rng)
     (define arrow-star? (and (Rest? rst) (> (length (Rest-tys rst)) 1)))
     (define dom-sexps (map type->sexp dom))
     (append
      (if arrow-star?
          (list '->* dom-sexps)
          (cons '-> dom-sexps))
      ;; Format keyword types as strings because the square
      ;; brackets are significant for printing. Note that
      ;; as long as the resulting s-expressions are `display`ed
      ;; this is fine, though it may not pretty-print well.
      (for/list ([kw (in-list kws)])
        (match kw
          [(Keyword: k t req?)
           (if req?
               (format "~a ~a" k (type->sexp t))
               (format "[~a ~a]" k (type->sexp t)))]))
      (match rst
        [(Rest: (list rst-t)) `(,(type->sexp rst-t) *)]
        [(Rest: rst-ts) `(#:rest-star ,(map type->sexp rst-ts))]
        [(RestDots: dty dbound)
         `(,(type->sexp dty) ... ,dbound)]
        [_ null])
      (match rng
        [(AnyValues: (? TrueProp?)) '(AnyValues)]
        [(AnyValues: p) `(AnyValues : ,(prop->sexp p))]
        [(Values: (or (list (Result: t (PropSet: (? TrueProp?) (? TrueProp?)) (? Empty?)))
                      (list (Result: (and (== -False) t) (PropSet: (? FalseProp?) (? TrueProp?)) (? Empty?)))
                      (list (Result: (and t (app (λ (t) (overlap? t -False)) #f))
                                     (PropSet: (? TrueProp?) (? FalseProp?))
                                     (? Empty?)))))
         (list (type->sexp t))]
        [(Values: (list (Result: t
                                 (PropSet:
                                  (TypeProp: (and o (Path: pth1 (cons 0 0))) ft1)
                                  (NotTypeProp: (Path: pth2 (cons 0 0)) ft2))
                                 (? Empty?))))
         ;; Only print a simple prop for single argument functions,
         ;; since parse-type only accepts simple latent props on single
         ;; argument functions.
         #:when (and (equal? pth1 pth2)
                     (equal? ft1 ft2)
                     (= 1 (length dom)))
         (if (null? pth1)
             `(,(type->sexp t) : ,(type->sexp ft1))
             `(,(type->sexp t) : ,(type->sexp ft1) @
               ,(object->sexp o)))]
        ;; Print asymmetric props with only a positive prop as a
        ;; special case (even when complex printing is off) because it's
        ;; useful to users who use functions like `prop`.
        [(Values: (list (Result: t
                                 (PropSet:
                                  (TypeProp: (Path: '() (cons 0 0)) ft)
                                  (? TrueProp?))
                                 (? Empty?))))
         #:when (= 1 (length dom))
         `(,(type->sexp t) : #:+ ,(type->sexp ft))]
        [(Values: (list (Result: t ps (? Empty?))))
         (if (print-complex-props?)
             `(,(type->sexp t) : ,(propset->sexp ps))
             (list (type->sexp t)))]
        [(Values: (list (Result: t ps o)))
         (if (print-complex-props?)
             `(,(type->sexp t) : ,(propset->sexp ps) ,(object->sexp o))
             (list (type->sexp t)))]
        [_ (list (values->sexp rng))]))]
    [else `(Unknown Function Type: ,(struct->vector arr))]))

;; format->* : (Listof arr) -> S-Expression
;; Format arrs that correspond to a ->* type
(define (format->* arrs)
  ;; see type-contract.rkt, which does something similar and this code
  ;; was stolen from/inspired by/etc.
  (match* ((first arrs) (last arrs))
    [((Arrow: first-dom _ kws rng)
      (Arrow: last-dom rst _ _))
     (define-values (mand-kws opt-kws) (partition-kws kws))
     (define opt-doms (drop last-dom (length first-dom)))
     `(->*
       ,(append* (for/list ([dom (in-list first-dom)])
                   (type->sexp dom))
                 (for/list ([mand-kw (in-list mand-kws)])
                   (match-define (Keyword: k t _) mand-kw)
                   (list k (type->sexp t))))
       ,(append* (for/list ([opt-dom (in-list opt-doms)])
                   (type->sexp opt-dom))
                 (for/list ([opt-kw (in-list opt-kws)])
                   (match-define (Keyword: k t _) opt-kw)
                   (list k (type->sexp t))))
       ,@(match rst
           [#f null]
           [(Rest: (list rst-t)) `(#:rest ,(type->sexp rst-t))]
           [(Rest: rst-ts) `(#:rest-star ,(map type->sexp rst-ts))])
       ,(values->sexp rng))]))

;; cover-case-lambda : (Listof arr) -> (Listof s-expression)
;; Try to cover a case-> type with ->* types
(define (cover-case-lambda arrs)
  ;; sublists : (Listof X) -> (Listof (List (Listof X) (Listof X) (Listof X)))
  ;; produce sublists of a list in decreasing order, also
  ;; returning the rest of the list before and after the
  ;; sublist for each.
  (define (sublists lst)
    (define (sublist-n n lst)
      (for/list ([to-drop (range (- (length lst) (- n 1)))])
        (define-values (pre mid) (split-at lst to-drop))
        (define-values (sub post) (split-at mid n))
        (list pre sub post)))
    (apply append (for/list ([i (range (length lst) 0 -1)])
                    (sublist-n i lst))))
  (let loop ([left-to-cover arrs])
    ;; try to match the largest sublists possible that correspond to
    ;; ->* types and then the remainder are formatted normally
    (define a-match
      (for/first ([sub (in-list (sublists left-to-cover))]
                  #:when (has-optional-args? (second sub)))
        sub))
    (cond [a-match
           (match-define (list pre sub post) a-match)
           (append (loop pre) (list (format->* sub)) (loop post))]
          [else (map arr->sexp left-to-cover)])))

;; case-lambda->sexp : Type -> S-expression
;; Convert a case-> type to an s-expression
(define (case-lambda->sexp type)
  (match type
    [(Fun: arrows)
     (match arrows
       [(list) '(case->)]
       [(list a) (arr->sexp a)]
       [(and arrs (list a b ...))
        (define cover (cover-case-lambda arrs))
        (if (> (length cover) 1)
            `(case-> ,@cover)
            (car cover))])]))

;; class->sexp : Class [#:object? Boolean] -> S-expression
;; Convert a class or object type to an s-expression
(define (class->sexp cls #:object? [object? #f])
  (match-define (Class: row-var inits fields methods augments init-rest) cls)
  (define row-var*
    (if (and row-var (F? row-var)) `(#:row-var ,(F-n row-var)) '()))
  (define inits*
    (if (or object? (null? inits))
        null
        (list
         (cons 'init
               (for/list ([init inits])
                 (match-define (list name type opt?) init)
                 (if opt?
                     (list name (type->sexp type) '#:optional)
                     (list name (type->sexp type))))))))
  (define fields*
    (if (null? fields)
        null
        (list
         (cons 'field
               (for/list ([name+type (in-list fields)])
                 (match-define (list name type) name+type)
                 `(,name ,(type->sexp type)))))))
  (define methods*
    (for/list ([name+type (in-list methods)])
      (match-define (list name type) name+type)
      `(,name ,(type->sexp type))))
  (define augments*
    (cond [(or object? (null? augments)) '()]
          [else (list (cons 'augment augments))]))
  (define init-rest*
    (if (and init-rest (not object?))
        (list `(init-rest ,init-rest))
        null))
  `(,(if object? 'Object 'Class)
    ,@row-var* ,@inits* ,@init-rest* ,@fields* ,@methods* ,@augments*))

;; result->sexp : Result -> S-expression
;; convert a result to an s-expression that can be printed
(define (result->sexp res)
  (match res
    [(Result: t
              (or 'none (PropSet: (? TrueProp?) (? TrueProp?)))
              (or 'none (? Empty?)))
     (type->sexp t)]
    [(Result: t ps (? Empty?)) `(,(type->sexp t) : ,(propset->sexp ps))]
    [(Result: t ps lo) `(,(type->sexp t) :
                         ,(propset->sexp ps) :
                         ,(object->sexp lo))]
    [else `(Unknown Result: ,(struct->vector res))]))

;; propset->sexp : Result -> S-expression
;; convert a prop set to an s-expression that can be printed
(define (propset->sexp ps)
  (match ps
    [(PropSet: thn els) `(,(prop->sexp thn) \| ,(prop->sexp els))]
    [else `(Unknown PropSet: ,(struct->vector ps))]))

;; values->sexp : SomeValues -> S-expression
;; convert a values to an s-expression that can be printed
(define (values->sexp v)
  (match v
    [(AnyValues: (? TrueProp?)) 'AnyValues]
    [(AnyValues: p) `(AnyValues : ,(prop->sexp p))]
    [(Values: (list v)) v]
    [(Values: vals) (cons 'values (map result->sexp vals))]
    [(ValuesDots: v dty dbound)
     (cons 'values (append (map result->sexp v)
                           (list (type->sexp dty) '... dbound)))]
    [else `(Unknown SomeValues: ,(struct->vector v))]))

;; signature->sexp : Signature -> S-expression
;; convert a values to an s-expression that can be printed
(define (signature->sexp s)
  (match s
    [(Signature: name extends mapping)
     (syntax->datum name)]
    [else `(Unknown Signature: ,(struct->vector s))]))


;; type->sexp : Type -> S-expression
;; convert a type to an s-expression that can be printed
(define (type->sexp type [ignored-names '()])
  (define (t->s type)
    (parameterize ([current-print-type-fuel
                    (sub1 (current-print-type-fuel))])
      (type->sexp type)))
  (define (tuple? t)
    (match t
      [(Pair: a (? tuple?)) #t]
      [(== -Null) #t]
      [_ #f]))
  (define (improper-tuple? t)
    (let loop ([t t]
               [depth 0])
      (match t
        [(Pair: a rst) (loop rst (add1 depth))]
        [_ (>= depth 2)])))
  (define (tuple-elems t)
    (match t
      [(Pair: a e) (cons a (tuple-elems e))]
      [(== -Null) null]))
  (define (improper-tuple-elems t)
    (match t
      [(Pair: a e) (cons a (improper-tuple-elems e))]
      [end (list end)]))
  (match type
    [(Univ:) 'Any]
    [(Bottom:) 'Nothing]
    ;; struct names are just printed as the original syntax
    [(Name/struct: id) (syntax-e id)]
    ;; If a type has a name, then print it with that name.
    ;; However, we expand the alias in some cases
    ;; (i.e., the fuel is > 0) for the :type form.
    [(app has-name? (? values names))
     (=> fail)
     (when (not (null? ignored-names)) (fail))
     (define fuel (current-print-type-fuel))
     (cond [(> fuel 0)
            (parameterize ([current-print-type-fuel (sub1 fuel)])
              ;; if we still have fuel, print the expanded type and
              ;; add the name to the ignored list so that the union
              ;; printer does not try to print with the name.
              (type->sexp (if (Name? type) (resolve type) type)
                          (append names ignored-names)))]
           [else
            ;; to allow :type to cue the user on unexpanded aliases
            (when (or (Union? type) (BaseUnion? type)) ; only unions can be expanded
              (set-box! (current-print-unexpanded)
                        (cons (car names) (unbox (current-print-unexpanded)))))
            (car names)])]
    [(StructType: (Struct: nm _ _ _ _ _ _)) `(StructType ,(syntax-e nm))]
    ;; this case occurs if the contained type is a type variable
    [(StructType: ty) `(Struct-Type ,(t->s ty))]
    [(StructTypeTop:) 'Struct-TypeTop]
    [(StructTop: (Struct: nm _ _ _ _ _ _)) `(Struct ,(syntax-e nm))]
    [(Prefab: key field-types)
     `(Prefab ,(abbreviate-prefab-key key)
              ,@(map t->s field-types))]
    [(PrefabTop: key)
     `(PrefabTop ,(abbreviate-prefab-key key)
                 ,(prefab-key->field-count key))]
    [(BoxTop:) 'BoxTop]
    [(Weak-BoxTop:) 'Weak-BoxTop]
    [(ChannelTop:) 'ChannelTop]
    [(Async-ChannelTop:) 'Async-ChannelTop]
    [(ThreadCellTop:) 'ThreadCellTop]
    [(MPairTop:) 'MPairTop]
    [(Prompt-TagTop:) 'Prompt-TagTop]
    [(Struct-Property: ty pred-id) `(Struct-Property ,(t->s ty))]
    [(Has-Struct-Property: sym) `(Has-Struct-Property ,(syntax-e sym))]
    [(Continuation-Mark-KeyTop:) 'Continuation-Mark-KeyTop]
    [(App: rator rands)
     (list* (type->sexp rator) (map type->sexp rands))]
    ;; Special cases for lists. Avoid printing with these cases if the
    ;; element type refers to the Mu variable (otherwise it prints the
    ;; type variable with no binding).
    [(SimpleListof: elem-ty)
     ;; in the 'elem-ty' type
     `(Listof ,(t->s elem-ty))]
    [(SimpleMListof: elem-ty)
     `(MListof ,(t->s elem-ty))]
    [(? tuple? t)
     `(List ,@(map type->sexp (tuple-elems t)))]
    [(? improper-tuple? t)
     `(List* ,@(map type->sexp (improper-tuple-elems t)))]
    [(Opaque: pred) `(Opaque ,(syntax->datum pred))]
    [(Struct: nm par (list (fld: t _ _) ...) proc _ _ properties)
     `#(,(string->symbol (format "struct:~a" (syntax-e nm)))
        ,(map t->s t)
        ,@(if proc (list (t->s proc)) null)
        ,@(free-id-set->list properties))]
    [(? Fun?)
     (parameterize ([current-print-type-fuel
                     (sub1 (current-print-type-fuel))])
       (case-lambda->sexp type))]
    [(? Arrow?) `(Arrow ,(arr->sexp type))]
    [(Immutable-Vector: e) `(Immutable-Vectorof ,(t->s e))]
    [(Mutable-Vector: e) `(Mutable-Vectorof ,(t->s e))]
    [(Mutable-VectorTop:) 'Mutable-VectorTop:]
    [(Immutable-HeterogeneousVector: e) `(Immutable-Vector ,@(map t->s e))]
    [(Mutable-HeterogeneousVector: e) `(Mutable-Vector ,@(map t->s e))]
    [(Box: e) `(Boxof ,(t->s e))]
    [(Weak-Box: e) `(Weak-Boxof ,(t->s e))]
    [(Future: e) `(Futureof ,(t->s e))]
    [(Channel: e) `(Channelof ,(t->s e))]
    [(Async-Channel: e) `(Async-Channelof ,(t->s e))]
    [(ThreadCell: e) `(ThreadCellof ,(t->s e))]
    [(Promise: e) `(Promise ,(t->s e))]
    [(Ephemeron: e) `(Ephemeronof ,(t->s e))]
    [(CustodianBox: e) `(CustodianBoxof ,(t->s e))]
    [(Set: e) `(Setof ,(t->s e))]
    [(Evt: r) `(Evtof ,(t->s r))]
    [(? Union? (app normalize-type type))
     (match type
       [(Union-all-flat: ts)
        (define-values (covered remaining) (cover-union type ts ignored-names))
        (match (sort (append covered (map t->s remaining)) primitive<=?)
          [(list t) t]
          [ts (cons 'U ts)])]
       [_ (t->s type)])]
    [(BaseUnion-bases: bs)
     (define-values (covered remaining) (cover-union type bs ignored-names))
     (cons 'U (sort (append covered (map t->s remaining)) primitive<=?))]
    [(Refine: raw-ty raw-prop)
     (with-printable-names 1 names
       (define ty (instantiate-obj raw-ty names))
       (define prop (instantiate-obj raw-prop names))
       `(Refine [,(name-ref->sexp (car names)) : ,ty] ,(prop->sexp prop)))]
    [(Intersection: elems _)
     (cons '∩ (sort (map t->s elems) primitive<=?))]
    ;; format as a string to preserve reader abbreviations and primitive
    ;; values like characters (when `display`ed)
    ;; (comes after Intersection since Val-able will match
    ;;  when an element of an intersection is a val)
    [(Val-able: v) (cond [(void? v) 'Void]
                         [else (format "~v" v)])]
    [(? Base?) (Base-name type)]
    [(Pair: l r) `(Pairof ,(t->s l) ,(t->s r))]
    [(ListDots: dty dbound) `(List ,(t->s dty) ... ,dbound)]
    [(F: nm)
     (cond
       [(eq? nm imp-var) "Imp"]
       [(eq? nm self-var) "Self"]
       [else nm])]
    [(Param: in out)
     (if (equal? in out)
         `(Parameterof ,(t->s in))
         `(Parameterof ,(t->s in) ,(t->s out)))]
    [(Mutable-HashTable: k v) `(Mutable-HashTable ,(t->s k) ,(t->s v))]
    [(Mutable-HashTableTop:) 'Mutable-HashTableTop]
    [(Immutable-HashTable: k v) `(Immutable-HashTable ,(t->s k) ,(t->s v))]
    [(Weak-HashTable: k v) `(Weak-HashTable ,(t->s k) ,(t->s v))]
    [(Weak-HashTableTop:) 'Weak-HashTableTop]
    [(Continuation-Mark-Keyof: rhs)
     `(Continuation-Mark-Keyof ,(t->s rhs))]
    [(Prompt-Tagof: body handler)
     `(Prompt-Tagof ,(t->s body) ,(t->s handler))]
    [(Poly-names: names body)
     `(All ,names ,(t->s body))]
    [(PolyDots-names: (list names ... dotted) body)
     `(All ,(append names (list dotted '...)) ,(t->s body))]
    ;; FIXME: should this print constraints too
    [(PolyRow-names: names _ body)
     `(All (,(car names) #:row) ,(t->s body))]
    [(Exist-names: n body)
     `(Exist ,n ,(t->s body))]
    ;; x1 --> ()
    [(Mu-unsafe:
      (Syntax: (Union: (== (Un -Number -Boolean -Symbol -String))
                       ts)))
     #:when (and (= 4 (length ts))
                 (member (-vec (make-B 0)) ts)
                 (member (-box (make-B 0)) ts)
                 (let ([ts (remove (-box (make-B 0))
                                   (remove (-vec (make-B 0)) ts))])
                   (match ts
                     [(list-no-order (Mu-unsafe:
                                      (Union: (== -Null)
                                              (list (Pair: (B: 1) (B: 0)))))
                                     (Mu-unsafe:
                                      (Union: (== -Bottom)
                                              (list-no-order
                                               (B: 1)
                                               (Pair: (B: 1) (B: 0))))))
                      #t]
                     [_ #f])))
     'Syntax]
    [(Mu-maybe-name: name (? Type? body))
     `(Rec ,name ,(t->s body))]
    [(Mu-unsafe: raw-body)
     (with-printable-names 1 name-ids
       (let ([names (for/list ([id (in-list name-ids)])
                      (make-F (syntax-e id)))])
         `(Rec ,(first names)
               ,(t->s (instantiate-type raw-body names)))))]
    [(B: idx) `(B ,idx)]
    [(Syntax: t) `(Syntaxof ,(t->s t))]
    [(Instance: (and (? has-name?) cls)) `(Instance ,(t->s cls))]
    [(Instance: (? Class? cls)) (class->sexp cls #:object? #t)]
    [(Instance: t) `(Instance ,(t->s t))] ; for cases like Error
    [(ClassTop:) 'ClassTop]
    [(? Class?) (class->sexp type)]
    [(Unit: (list imports ...) (list exports ...) (list init-depends ...) body)
     `(Unit
       (import ,@(map signature->sexp imports))
       (export ,@(map signature->sexp exports))
       (init-depend ,@(map signature->sexp init-depends))
       ,(values->sexp body))]
    [(UnitTop:) 'UnitTop]
    [(MPair: s t) `(MPairof ,(t->s s) ,(t->s t))]
    [(Refinement: parent p?)
     `(Refinement ,(t->s parent) ,(syntax-e p?))]
    [(Sequence: ts)
     `(Sequenceof ,@(map t->s ts))]
    [(SequenceDots: ts dty dbound)
     `(Sequenceof ,@(map t->s ts) ,(t->s dty) ... ,dbound)]
    [(SequenceTop:) 'SequenceTop]
    [(Error:) 'Error]
    ;[(fld: t a m) `(fld ,(type->sexp t))]
    [(Distinction: name sym ty) ; from define-new-subtype
     name]
    [(DepFun: raw-dom raw-pre raw-rng)
     (with-printable-names (length raw-dom) ids
       (define dom (for/list ([d (in-list raw-dom)])
                     (instantiate-obj d ids)))
       (define pre (instantiate-obj raw-pre ids))
       (define rng (instantiate-obj raw-rng ids))
       (define (arg-id? id) (member id ids free-identifier=?))
       (define pre-deps (map name-ref->sexp
                             (filter arg-id? (free-ids pre))))
       `(-> ,(for/list ([id (in-list ids)]
                        [d (in-list dom)])
               (define deps (map name-ref->sexp
                                 (filter arg-id? (free-ids d))))
               `(,(syntax-e id)
                 :
                 ,@(if (null? deps)
                       '()
                       (list deps))
                 ,(t->s d)))
            ,@(cond
                [(TrueProp? pre) '()]
                [(null? pre-deps) `(#:pre ,(prop->sexp pre))]
                [else `(#:pre ,pre-deps ,(prop->sexp pre))])
            ,(values->sexp rng)))]
    [else `(Unknown Type: ,(struct->vector type))]))



(define-syntax (define-debug-printer stx)
  (syntax-parse stx
    [(_ debug-printer:id)
     #:when (eq? printer-type 'debug)
     #'(begin
         (require racket/pretty)
         (require mzlib/pconvert)

         (define (converter v basic sub)
           (define (gen-constructor sym)
             (string->symbol (string-append "make-" (substring (symbol->string sym) 7))))
           (match v
             [(? Rep? rep)
              `(,(gen-constructor (car (vector->list (struct->vector rep))))
                ,@(map sub (Rep-values rep)))]
             [_ (basic v)]))

         (define (debug-printer v port write?)
           ((if write? pretty-write pretty-print)
            (parameterize ((current-print-convert-hook converter))
              (print-convert v))
            port)))]
    [_ #'(begin)]))

(define-debug-printer debug-printer)

;; debug-pretty-format-type : Type -> String
;; Debugging mode for pretty printing types, which just uses
;; the debug printer above. Ignores kw argument. Only defined
;; in debug printing mode.
(define-syntax (define-debug-pretty-format-type stx)
  (syntax-parse stx
    [(_ debug-pretty-format-type:id)
     (if (eq? printer-type 'debug)
       #'(define (debug-pretty-format-type type #:indent [indent 0])
           (define out (open-output-string))
           (debug-printer type out #t)
           (get-output-string out))
       #'(void))]))

(define-debug-pretty-format-type debug-pretty-format-type)
