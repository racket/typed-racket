#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer infer-in]))
(require racket/match racket/list
         (only-in unstable/list list-update)
         (for-syntax racket/base syntax/parse)
         (contract-req)
         (infer-in infer)
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         (types tc-result resolve subtype remove-intersect union filter-ops
                numeric-tower)
         (env type-env-structs lexical-env)
         (logic prop-ops)
         (rename-in (types abbrev)
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (typecheck tc-metafunctions))

(provide with-lexical-env/extend-props 
         with-lexical-env/extend-types
         with-lexical-env/extend-types+aliases+props
         with-lexical-env/naive-extend-types
         update
         env-extend-types)


(define/cond-contract (update t ft pos? lo)
  (Type/c Type/c boolean? (listof PathElem?) . -> . Type/c)
  ;; build-type: build a type while propogating bottom
  (define (build-type constructor . args)
    (if (memf Bottom? args) -Bottom (apply constructor args)))
  (match* ((resolve t) lo)
    ;; pair ops
    [((Pair: t s) (list rst ... (CarPE:)))
     (build-type -pair (update t ft pos? rst) s)]
    [((Pair: t s) (list rst ... (CdrPE:)))
     (build-type -pair t (update s ft pos? rst))]

    ;; syntax ops
    [((Syntax: t) (list rst ... (SyntaxPE:)))
     (build-type -Syntax (update t ft pos? rst))]

    ;; promise op
    [((Promise: t) (list rst ... (ForcePE:)))
     (build-type -Promise (update t ft pos? rst))]

    ;; struct ops
    [((Struct: nm par flds proc poly pred)
      (list rst ... (StructPE: (? (lambda (s) (subtype t s)) s) idx)))
     ;; note: this updates fields regardless of whether or not they are
     ;; a polymorphic field. Because subtyping is nominal and accessor 
     ;; functions do not reflect this, this behavior is unobservable
     ;; except when an a variable aliases the field in a let binding
     (let*-values ([(lhs rhs) (split-at flds idx)]
                   [(ty* acc-id) (match rhs
                                   [(cons (fld: ty acc-id #f) _)
                                    (values (update ty ft pos? rst) acc-id)]
                                   [_ (int-err "update on mutable struct field")])]) 
       (cond 
        [(Bottom? ty*) ty*]
        [else (let ([flds* (append lhs (cons (make-fld ty* acc-id #f) (cdr rhs)))])
                (make-Struct nm par flds* proc poly pred))]))]

    ;; otherwise
    [(t '())
     (if pos?
         (restrict t ft)
         (remove t ft))]
    [((Union: ts) lo)
     (apply Un (map (λ (t) (update t ft pos? lo)) ts))]
    [(t* lo)
     ;; This likely comes up with (-lst t) and we need to improve the system to make sure this case
     ;; dosen't happen
     ;;(int-err "update along ill-typed path: ~a ~a ~a" t t* lo)
     t]))

(define (env-extend-types env ids types)
  (define-values (ids/ts* pss) 
    (for/lists (ids/ts ps) 
      ([id (in-list ids)] [t (in-list types)])
      (let-values ([(t* ps) (extract-props-from-type id t)])
        (values (cons id t*) ps))))
  (cond
    [(for/or ([id/t (in-list ids/ts*)]) (type-equal? (cdr id/t) -Bottom))
     #f]
    [else 
     (define ps (apply append pss))
     (define-values (new-env _) 
       (env+props (naive-extend/types (lexical-env) ids/ts*)
                  ps))
     new-env]))

;; extend the environment with a list of propositions
;; Returns #f if anything becomes (U)
(define (env+props env fs)
  (let/ec exit*
    (define (exit) (exit* #f empty))
    (define-values (props atoms slis) 
      (combine-props (apply append (map flatten-nested-props fs)) 
                     (env-props env)
                     exit))
    (values
     (for/fold ([Γ (replace-SLIs (replace-props env props) slis)]) ([f (in-list atoms)])
       (match f
         [(or (TypeFilter: ft (Path: lo x)) (NotTypeFilter: ft (Path: lo x)))
          (update-type/lexical
           (lambda (x t)
             (define new-t (update t ft (TypeFilter? f) lo))
             (when (type-equal? new-t -Bottom)
               (exit))
             new-t)
           x Γ)]
         [(TypeFilter: ft (? LExp? l))
          (if (subtype ft -Integer #:obj l)
              Γ
              (exit))]
         [(NotTypeFilter: ft (? LExp? l))
          (if (subtype -Integer ft #:obj l)
              (exit)
              Γ)]
         [_ Γ]))
     atoms)))

;; run code in an extended env and with replaced props. Requires the body to return a tc-results.
;; TODO make this only add the new prop instead of the entire environment once tc-id is fixed to
;; include the interesting props in its filter.
;; WARNING: this may bail out when code is unreachable
(define-syntax (with-lexical-env/extend-props stx)
  (define-splicing-syntax-class unreachable?
    (pattern (~seq #:unreachable form:expr))
    (pattern (~seq) #:with form #'(begin)))
  (syntax-parse stx
    [(_ ps:expr u:unreachable? . bodies)
     #'(let*-values ([(new-env atoms) (env+props (lexical-env) ps)])
         (if new-env
             (with-lexical-env new-env
               (add-unconditional-prop 
                (let () . bodies) 
                (apply -and (append atoms (env-props new-env)))))
             ;; unreachable, bail out
             (let ()
               u.form
               (ret -Bottom))))]))

;; run code in an extended env and with replaced props. Requires the body to return a tc-results.
;; TODO make this only add the new prop instead of the entire environment once tc-id is fixed to
;; include the interesting props in its filter.
;; WARNING: this may bail out when code is unreachable
(define-syntax (with-lexical-env/extend-types stx)
  (define-splicing-syntax-class unreachable?
    (pattern (~seq #:unreachable form:expr)))
  (syntax-parse stx
    [(_ ids:expr types:expr u:unreachable? . bodies)
     #'(let ()
         (define-values (ids/ts* pss) 
           (for/lists (ids/ts ps) 
             ([id (in-list ids)] [t (in-list types)])
             (let-values ([(t* ps) (extract-props-from-type id t)])
               (values (cons id t*) ps))))
         (cond
           [(for/or ([id/t (in-list ids/ts*)]) 
              (type-equal? (cdr id/t) -Bottom))
            ;; unreachable, bail out
            u.form]
           [else
            (let*-values 
                ([(ps) (apply append pss)]
                 [(new-env atoms) (env+props (naive-extend/types (lexical-env) ids/ts*)
                                             ps)]
                 [(new-env) (and new-env (replace-props new-env (append atoms (env-props new-env))))])
              (if new-env
                  (with-lexical-env 
                   new-env
                   (let () . bodies))
                  ;; unreachable, bail out
                  u.form))]))]))


;; run code in an extended env and with replaced props. Requires the body to return a tc-results.
;; TODO make this only add the new prop instead of the entire environment once tc-id is fixed to
;; include the interesting props in its filter.
;; WARNING: this may bail out when code is unreachable
(define-syntax (with-lexical-env/extend-types+aliases+props stx)
  (define-splicing-syntax-class unreachable?
    (pattern (~seq #:unreachable form:expr)))
  (syntax-parse stx
    [(_ ids:expr types:expr aliases:expr ps:expr u:unreachable? . bodies)
     #'(let*-values 
           ([(ids/ts* ids/als pss)
             (for/fold ([ids/ts null] [ids/als null] [pss null]) 
                       ([id (in-list ids)] [t (in-list types)] [o (in-list aliases)])
               (let-values
                   ([(t* ps*) (extract-props-from-type id t)])
                 (match o
                   [(Empty:)
                    ;; no alias, so just record the type and props as usual
                    (values `((,id . ,t*) . ,ids/ts) 
                            ids/als
                            (cons ps* pss))]
                   [(Path: '() id*)
                    ;; id is aliased to an identifier
                    ;; record the alias relation *and* type of that alias id along w/ props
                    (values `((,id* . ,t*) . ,ids/ts) 
                            `((,id . ,o) . ,ids/als)
                            (cons ps* pss))]
                   ;; standard aliasing, just record the type and the alias
                   [(or (? Path? o) (? LExp? o)) 
                    (values ids/ts
                            `((,id . ,o) . ,ids/als)
                            (cons ps* pss))])))])
         (cond
           [(for/or ([id/t (in-list ids/ts*)]) (type-equal? (cdr id/t) -Bottom))
            ;; unreachable, bail out
            u.form]
           [else 
            (let*-values
                ([(all-ps) (apply append (cons ps pss))]
                 [(env) (naive-extend/types (lexical-env) ids/ts*)] 
                 [(env) (and env (extend/aliases env ids/als))]
                 [(new-env atoms) (if env (env+props env all-ps) (values #f '()))]
                 [(new-env) (and new-env 
                                 (replace-props new-env 
                                                (append atoms (env-props new-env))))])
              (if new-env
                  (with-lexical-env new-env (let () . bodies))
                  ;; unreachable, bail out
                  (let () u.form)))]))]))



;; run code in an extended env
;; WARNING! does not reason about nested props in refinements
(define-syntax-rule (with-lexical-env/naive-extend-types ids types . b)
  (with-lexical-env (naive-extend/types (lexical-env) (map cons ids types)) . b))


