#lang racket/base

#|

This file defines two sorts of primitives. All of them are provided into any module using
the typed racket language.

1. macros for defining type annotated code.
   this includes: lambda:, define:, etc
   potentially, these macros should be replacements for the Racket ones in the user program
   however, it's nice to be able to use both when the sugar is more limited
   for example: (define ((f x) y) (+ x y))

   Some of these are provided even in `typed/racket/base` where the corresponding
   untyped version is not provided by `racket/base` (such as `for/set`).

2. macros for defining 'magic' code
   examples: define-typed-struct, require/typed
   these expand into (ignored) mzscheme code, and declarations that a typechecker understands
   in order to protect the declarations, they are wrapped in `#%app void' so that local-expand
   of the module body will not expand them on the first pass of the macro expander (when the
   stop list is ignored)

3. contracted versions of built-in racket values such as parameters and prompt tags
   that are defined in "base-contracted.rkt"

   These are implemented using indirection so that contracts aren't loaded at runtime unless
   needed.

|#


(provide (except-out (all-defined-out) -let-internal define-for-variants
                     def-redirect
                     define-for*-variants with-handlers: define-for/acc:-variants
                     base-for/flvector: base-for/vector -lambda -define -do -let
                     -let* -let*-values -let-values -let/cc -let/ec -letrec -letrec-values)
         (all-from-out "top-interaction.rkt")
         (all-from-out "case-lambda.rkt")
         (all-from-out (submod "prims-contract.rkt" forms))
         define-type-alias
         define-typed-struct
         define-typed-struct/exec
         ann inst
         :
         (rename-out [define-typed-struct define-struct:]
                     [define-typed-struct define-struct]
                     [-struct struct]
                     [-struct struct:]
                     [lambda: λ:]
                     [-lambda lambda]
                     [-lambda λ]
                     [-define define]
                     [-let let]
                     [-let* let*]
                     [-letrec letrec]
                     [-let-values let-values]
                     [-letrec-values letrec-values]
                     [-let/cc let/cc]
                     [-let/ec let/ec]
                     [-let let:]
                     [-let* let*:]
                     [-letrec letrec:]
                     [-let-values let-values:]
                     [-letrec-values letrec-values:]
                     [-let/cc let/cc:]
                     [-let/ec let/ec:]
                     [for: for]
                     [for/list: for/list]
                     [for/vector: for/vector]
                     [for/hash: for/hash]
                     [for/hasheq: for/hasheq]
                     [for/hasheqv: for/hasheqv]
                     [for/and: for/and]
                     [for/or: for/or]
                     [for/sum: for/sum]
                     [for/product: for/product]
                     [for/lists: for/lists]
                     [for/first: for/first]
                     [for/last: for/last]
                     [for/fold: for/fold]
                     [for*: for*]
                     [for*/list: for*/list]
                     [for*/lists: for*/lists]
                     [for*/vector: for*/vector]
                     [for*/hash: for*/hash]
                     [for*/hasheq: for*/hasheq]
                     [for*/hasheqv: for*/hasheqv]
                     [for*/and: for*/and]
                     [for*/or: for*/or]
                     [for*/sum: for*/sum]
                     [for*/product: for*/product]
                     [for*/first: for*/first]
                     [for*/last: for*/last]
                     [for*/fold: for*/fold]
                     [for/set: for/set]
                     [for*/set: for*/set]
                     [-do do]
                     [-do do:]
                     [with-handlers: with-handlers]
                     [define-typed-struct/exec define-struct/exec:]
                     [define-typed-struct/exec define-struct/exec]))

(require "colon.rkt"
         "top-interaction.rkt"
         "base-types.rkt"
         "base-types-extra.rkt"
         "case-lambda.rkt"
         "prims-struct.rkt"
         "ann-inst.rkt"
         racket/unsafe/ops
         racket/flonum ; for for/flvector and for*/flvector
         racket/extflonum ; for for/extflvector and for*/extflvector
         (only-in "../types/numeric-predicates.rkt" index?)
         (submod "../typecheck/internal-forms.rkt" forms)
         (submod "prims-contract.rkt" forms)
         ;; for binding comparisons
         (for-label (only-in "base-types-extra.rkt" Values)
                    (only-in racket/base values))
         (for-syntax
          racket/lazy-require
          syntax/parse/pre
          syntax/stx
          racket/list
          racket/syntax
          unstable/syntax
          racket/base
          (only-in "../typecheck/internal-forms.rkt" internal)
          "annotate-classes.rkt"
          "../utils/literal-syntax-class.rkt"
          "../private/parse-classes.rkt"
          "../private/syntax-properties.rkt"
          "for-clauses.rkt"))

(provide index?) ; useful for assert, and racket doesn't have it


;; This section reprovides (using the same submodule technique as for
;; contracted bindings in typed modules) values that are contracted
;; for _all_ typed programs.
(module+ #%contract-defs
  (require "base-contracted.rkt")
  (provide (all-from-out "base-contracted.rkt")))

(begin-for-syntax 
  (require racket/base "../utils/redirect-contract.rkt")
  (define varref (#%variable-reference))
  (define mk (make-make-redirect-to-contract varref)))

(define-syntax-rule (def-redirect id ...)
  (begin (define-syntax id (mk (quote-syntax id))) ... (provide id ...)))

(def-redirect default-continuation-prompt-tag)

;; Lazily loaded b/c they're only used sometimes, so we save a lot
;; of loading by not having them when they are unneeded
(begin-for-syntax 
  (lazy-require ["../rep/type-rep.rkt" (make-Opaque Error?)]
                ["../types/utils.rkt" (fv)]
                [syntax/define (normalize-definition)]
                [typed-racket/private/parse-type (parse-type)]
                [typed-racket/env/type-alias-env (register-resolved-type-alias)]))

(define-for-syntax (with-type* expr ty)
  (with-type #`(ann #,expr #,ty)))

(define-syntax (plambda: stx)
  (syntax-parse stx
    [(plambda: tvars:type-variables formals . body)
     (plambda-property
       (syntax/loc stx (lambda: formals . body))
       #'(tvars.vars ...)) ]))

(define-syntax (popt-lambda: stx)
  (syntax-parse stx
    [(popt-lambda: tvars:type-variables formals . body)
     (plambda-property
       (syntax/loc stx (opt-lambda: formals . body))
       #'(tvars.vars ...))]))

(define-syntax (pdefine: stx)
  (syntax-parse stx #:literals (:)
    [(pdefine: tvars:type-variables (nm:id . formals:annotated-formals) : ret-ty . body)
     (with-syntax ([type (syntax/loc #'ret-ty (All (tvars.vars ...) (formals.arg-ty ... -> ret-ty)))])
       (syntax/loc stx
         (begin
          (: nm : type)
          (define (nm . formals.ann-formals) . body))))]))

(define-syntax (lambda: stx)
  (syntax-parse stx
    [(lambda: formals:annotated-formals . body)
     (syntax/loc stx (-lambda formals.ann-formals . body))]))

(define-syntax (opt-lambda: stx)
  (syntax-parse stx
    [(opt-lambda: formals:opt-lambda-annotated-formals . body)
     (syntax/loc stx (-lambda formals.ann-formals . body))]))

(define-syntaxes (-let-internal -let* -letrec)
  (let ([mk (lambda (form)
              (lambda (stx)
                (syntax-parse stx
                  [(_ (bs:optionally-annotated-binding ...) . body)
                   (quasisyntax/loc stx (#,form (bs.binding ...) . body))])))])
    (values (mk #'let) (mk #'let*) (mk #'letrec))))

(define-syntaxes (-let-values -let*-values -letrec-values)
  (let ([mk (lambda (form)
              (lambda (stx)
                (syntax-parse stx
                  [(_ (bs:optionally-annotated-values-binding ...) . body)
                   (quasisyntax/loc stx (#,form (bs.binding ...) . body))])))])
    (values (mk #'let-values) (mk #'let*-values) (mk #'letrec-values))))

(define-syntax (-let stx)
  (syntax-parse stx #:literals (:)
    [(-let nm:id ~! ; named let:
           (~and (~seq (~optional (~seq : ret-ty))
                       (bs:optionally-annotated-binding ...) body ...)
                 (~seq rest ...)))
     (syntax-parse #'(rest ...)
       #:literals (:)
       [(: ret-ty (bs:annotated-binding ...) . body)
        (quasisyntax/loc stx
          ((-letrec ([nm : (bs.ty ... -> ret-ty)
                         #,(quasisyntax/loc stx
                             (lambda (bs.ann-name ...) . #,(syntax/loc stx body)))])
                    #,(quasisyntax/loc stx nm))
           bs.rhs ...))]
       [(: ret-ty (bs:optionally-annotated-binding ...) body ... bod)
        (quasisyntax/loc stx
          (ann
           ((letrec ([nm #,(quasisyntax/loc stx
                             (lambda (bs.ann-name ...) body ... (ann #,(syntax/loc stx bod) ret-ty)))])
              #,(quasisyntax/loc stx nm))
            bs.rhs ...)
           ret-ty))]
       [((bs:optionally-annotated-binding ...) . body)
        (quasisyntax/loc stx
          ((letrec ([nm #,(quasisyntax/loc stx
                            (lambda (bs.ann-name ...) . #,(syntax/loc stx body)))])
             #,(quasisyntax/loc stx nm))
           bs.rhs ...))])]
    [(-let vars:lambda-type-vars
           ([bn:optionally-annotated-name e] ...)
           . rest)
     (define/with-syntax (bn* ...)
       ;; singleton names go to just the name
       (for/list ([bn (in-list (syntax->list #'(bn ...)))])
         (if (empty? (stx-cdr bn))
             (stx-car bn)
             bn)))
     (quasisyntax/loc stx ((-lambda #,@(syntax vars) (bn* ...) . rest) e ...))]
    [(-let . rest)
     (syntax/loc stx (-let-internal . rest))]))

(define-syntax (plet: stx)
  (syntax-parse stx #:literals (:)
    [(_ (A:id ...) ([bn:optionally-annotated-name e] ...) . body)
     (syntax/loc stx
       ((plambda: (A ...) (bn ...) . body) e ...))]))

(define-syntax (with-handlers: stx)
  (syntax-parse stx
    [(_ ([pred? action] ...) . body)
     (with-syntax ([(pred?* ...)
                    (for/list ([(pred? idx) (in-indexed (syntax->list #'(pred? ...)))])
                      (exn-predicate-property pred? idx))]
                   [(action* ...)
                    (for/list ([(action idx) (in-indexed (syntax->list #'(action ...)))])
                      (exn-handler-property action idx))]
                   [body* (exn-body #'(let-values () . body))])
       (exn-handlers (quasisyntax/loc stx
                       (with-handlers ([pred?* action*] ...) body*))))]))


(define-syntax (-do stx)
  (syntax-parse stx #:literals (:)
    [(_ (~optional (~seq : ty) #:defaults ([ty #f]))
        ((var:optionally-annotated-name rest ...) ...)
        (stop?:expr ret ...)
        c:expr ...)
     (define do-stx
       (syntax/loc stx
         (do ((var.ann-name rest ...) ...)
             (stop? ret ...)
           c ...)))
     (if (attribute ty)
         (quasisyntax/loc stx
           (ann #,do-stx #,(attribute ty)))
         do-stx)]))

;; we need handle #:when clauses manually because we need to annotate
;; the type of each nested for
(define-syntax (for: stx)
  (syntax-parse stx #:literals (: Void)
    ;; the annotation is not necessary (always of Void type), but kept
    ;; for consistency with the other for: macros
    [(_ (~optional (~seq : Void))
        clauses
        (~optional (~seq : Void)) ; can be either before or after
        ;; c is not always an expression, could be a break-clause
        c ...) ; no need to annotate the type, it's always Void
     (let ((body #'(; break-clause ...
                    c ...)))
       (let loop ((clauses #'clauses))
         (define-splicing-syntax-class for-clause
           ;; single-valued seq-expr
           ;; unlike the definitions in for-clauses.rkt, this does not include
           ;; #:when clauses, which are handled separately here
           (pattern (~seq (var:optionally-annotated-name seq-expr:expr))
                    #:with (expand ...) #'((var.ann-name seq-expr)))
           ;; multi-valued seq-expr
           (pattern ((v:optionally-annotated-formal ...) seq-expr:expr)
                    #:with (expand ...) #'(((v.ann-name ...) seq-expr)))
           ;; break-clause, pass it directly
           ;; Note: these don't ever typecheck
           (pattern (~seq (~and kw (~or #:break #:final)) guard-expr:expr)
                    #:with (expand ...) #'(kw guard-expr)))
         (define-syntax-class for-kw
           (pattern #:when
                    #:with replace-with #'when)
           (pattern #:unless
                    #:with replace-with #'unless))
         (syntax-parse clauses
           [(head:for-clause next:for-clause ... kw:for-kw rest ...)
            (add-ann
             (quasisyntax/loc stx
               (for
                (head.expand ... next.expand ... ...)
                #,(loop #'(kw rest ...))))
             #'Void)]
           [(head:for-clause ...) ; we reached the end
            (add-ann
             (quasisyntax/loc stx
               (for
                (head.expand ... ...)
                #,@body))
             #'Void)]
           [(kw:for-kw guard) ; we end on a keyword clause
            (quasisyntax/loc stx
              (kw.replace-with guard
                #,@body))]
           [(kw:for-kw guard rest ...)
            (quasisyntax/loc stx
              (kw.replace-with guard
                #,(loop #'(rest ...))))])))]))

(begin-for-syntax
  (define-splicing-syntax-class optional-standalone-annotation*
    #:attributes (ty annotate)
    (pattern :optional-standalone-annotation
      #:attr annotate (λ (stx) (if (attribute ty)
                                   (add-ann stx #'ty)
                                   stx)))))

;; Handling #:when clauses manually, like we do with for: above breaks
;; the semantics of for/list and co.
;; We must leave it to the untyped versions of the macros.
;; However, this means that some uses of these macros with #:when
;; clauses won't typecheck.
;; If the only #:when clause is the last clause, inference should work.
(define-for-syntax (define-for-variant name)
  (lambda (stx)
    (syntax-parse stx
      [(_ a1:optional-standalone-annotation*
          clause:for-clauses
          a2:optional-standalone-annotation* ; can be either before or after
          c ...) ; c is not always an expression, can be a break-clause
       ((attribute a1.annotate)
         ((attribute a2.annotate)
          (quasisyntax/loc stx
            (#,name
             (clause.expand ... ...)
             c ...))))])))

(define-syntax (define-for-variants stx)
  (syntax-parse stx
    [(_ (name untyped-name) ...)
     (quasisyntax/loc
         stx
       (begin (define-syntax name (define-for-variant #'untyped-name)) ...))]))

;; for/vector:, for/flvector:, for/and:, for/first: and
;; for/last:'s expansions can't currently be handled by the typechecker.
(define-for-variants
  (for/list: for/list)
  (for/and: for/and)
  (for/or: for/or)
  (for/first: for/first)
  (for/last: for/last))

;; Unlike with the above, the inferencer can handle any number of #:when
;; clauses with these 2.
(define-syntax (for/lists: stx)
  (syntax-parse stx
    [(_ a1:optional-standalone-annotation*
        (var:optionally-annotated-formal ...)
        clause:for-clauses
        a2:optional-standalone-annotation*
        c ...)
     (define all-typed? (andmap values (attribute var.ty)))
     (define for-stx
       (quasisyntax/loc stx
          (for/lists (var.ann-name ...)
            (clause.expand ... ...)
            c ...)))
     ((attribute a1.annotate)
      ((attribute a2.annotate)
       (if all-typed?
           (add-ann
            for-stx
            #'(values var.ty ...))
           for-stx)))]))
(define-syntax (for/fold: stx)
  (syntax-parse stx
    [(_ a1:optional-standalone-annotation*
        accum:accumulator-bindings
        clause:for-clauses
        a2:optional-standalone-annotation*
        c ...)
     (define all-typed? (andmap values (attribute accum.ty)))
     (define for-stx
       (quasisyntax/loc stx
         (for/fold ((accum.ann-name accum.init) ...)
                   (clause.expand ... ...)
           c ...)))
     ((attribute a1.annotate)
      ((attribute a2.annotate)
       (if all-typed?
           (add-ann
            for-stx
            #'(values accum.ty ...))
           for-stx)))]))

(define-syntax (for*: stx)
  (syntax-parse stx #:literals (: Void)
    [(_ (~optional (~seq : Void))
        clause:for-clauses
        (~optional (~seq : Void))
        c ...) ; c is not always an expression, can be a break-clause
     (quasisyntax/loc stx
       (for: (clause.expand* ... ...)
             c ...))]))

;; These currently only typecheck in very limited cases.
(define-for-syntax (define-for*-variant name)
  (lambda (stx)
    (syntax-parse stx
      [(_ a1:optional-standalone-annotation*
          clause:for-clauses
          a2:optional-standalone-annotation*
          c ...) ; c is not always an expression, can be a break-clause
       ((attribute a1.annotate)
        ((attribute a2.annotate)
         (quasisyntax/loc stx
           (#,name (clause.expand ... ...)
                   c ...))))])))
(define-syntax (define-for*-variants stx)
  (syntax-parse stx
    [(_ (name no-colon-name) ...)
     (quasisyntax/loc
         stx
       (begin (define-syntax name (define-for*-variant #'no-colon-name))
              ...))]))
(define-for*-variants
  (for*/and: for*/and)
  (for*/or: for*/or)
  (for*/first: for*/first)
  (for*/last: for*/last))

;; Like for/lists: and for/fold:, the inferencer can handle these correctly.
(define-syntax (for*/lists: stx)
  (syntax-parse stx
    [(_ a1:optional-standalone-annotation*
        ((var:optionally-annotated-name) ...)
        clause:for-clauses
        a2:optional-standalone-annotation*
        c ...)
     ((attribute a1.annotate)
      ((attribute a2.annotate)
       (add-ann
        (quasisyntax/loc stx
          (for/lists (var.ann-name ...)
              (clause.expand* ... ...)
            c ...))
        #'(values var.ty ...))))]))
(define-syntax (for*/fold: stx)
  (syntax-parse stx #:literals (:)
    [(_ a1:optional-standalone-annotation*
        ((var:optionally-annotated-name init:expr) ...)
        clause:for-clauses
        a2:optional-standalone-annotation*
        c ...)
     ((attribute a1.annotate)
      ((attribute a2.annotate)
       (add-ann
        (quasisyntax/loc stx
          (for/fold ((var.ann-name init) ...)
              (clause.expand* ... ...)
            c ...))
        #'(values var.ty ...))))]))

(define-for-syntax (define-for/acc:-variant for*? for/folder: for/folder op initial final)
  (lambda (stx)
    (syntax-parse stx #:literals (:)
      [(_ a1:optional-standalone-annotation*
          clause:for-clauses
          a2:optional-standalone-annotation*
          c ...) ; c is not always an expression, can be a break-clause
       (define a.ty (or (attribute a2.ty)
                        (attribute a1.ty)))
       (cond
        [a.ty
         ;; ty has to include exact 0, exact 1, null (sum/product/list respectively),
         ;; the initial value of the accumulator
         ;; (to be consistent with Racket semantics).
         ;; We can't just change the initial value to be 0.0 if we expect a
         ;; Float result. This is problematic in some cases e.g:
         ;; (for/sum: : Float ([i : Float '(1.1)] #:when (zero? (random 1))) i)
         (quasisyntax/loc stx
           (#,final
             (#,for/folder: : #,a.ty ([acc : #,a.ty #,initial])
                           (clause.expand ... ...)
                           (let ([new (let () c ...)])
                             (#,op acc new)))))]
        ;; With no annotation, try our luck with the core form.
        ;; Exact base cases cause problems, thus the additional
        ;; annotation on the accumulator above.
        [for*? ((define-for*-variant for/folder) stx)]
        [else ((define-for-variant for/folder) stx)])])))

(define-syntax (define-for/acc:-variants stx)
  (syntax-parse stx
    [(_ (name for/folder: for/folder for*? op initial final) ...)
     (quasisyntax/loc stx
       (begin (define-syntax name
                (define-for/acc:-variant
                  for*? #'for/folder: #'for/folder #'op #'initial #'final))
              ...))]))
(define-for/acc:-variants
  (for/sum: for/fold: for/sum #f + 0 #%expression)
  (for*/sum: for*/fold: for*/sum #t + 0 #%expression)
  (for*/list: for*/fold: for*/list #t (lambda (x y) (cons y x)) null reverse)
  (for/product: for/fold: for/product #f * 1 #%expression)
  (for*/product: for*/fold: for*/product #t * 1 #%expression))

;; originally, we made the mistake of providing these by default in typed/racket/base
;; so now we have this trickery here
;; This trickery is only used for `typed/racket/base`; `typed/racket` just provides the
;; sensible versions directly.
(define-syntaxes (for/set: for*/set:)
  (let ()
    (define ((mk id) stx)
      (syntax-case stx ()
        [(_ . r)
         #`(let ()
             (local-require (only-in (submod typed-racket/base-env/prims for-set) #,id))
             #,(quasisyntax/loc stx (#,id . r)))]))
    (values (mk (datum->syntax #'here 'for/set:))
            (mk (datum->syntax #'here 'for*/set:)))))

(module* for-set #f
  (require racket/set)
  (provide (rename-out [for/set: for/set]
                       [for*/set: for*/set])
           for/set: for*/set:)
  (define-for/acc:-variants
    (for/set: for/fold: for/set #f set-add (set) #%expression)
    (for*/set: for*/fold: for*/set #t set-add (set) #%expression)))

(define-for-syntax (define-for/hash:-variant hash-maker)
  (lambda (stx)
    (syntax-parse stx
      [(_ a1:optional-standalone-annotation*
          clause:for-clauses
          a2:optional-standalone-annotation*
          body ...) ; body is not always an expression, can be a break-clause
       (define a.ty (or (attribute a2.ty) (attribute a1.ty)))
       (if a.ty
           (quasisyntax/loc stx
             (for/fold: : #,a.ty
               ((return-hash : #,a.ty (ann (#,hash-maker null) #,a.ty)))
               (clause.expand ... ...)
               (let-values (((key val) (let () body ...)))
                 (hash-set return-hash key val))))
           (syntax/loc stx
             (for/hash (clause.expand ... ...)
               body ...)))])))

(define-syntax for/hash:    (define-for/hash:-variant #'make-immutable-hash))
(define-syntax for/hasheq:  (define-for/hash:-variant #'make-immutable-hasheq))
(define-syntax for/hasheqv: (define-for/hash:-variant #'make-immutable-hasheqv))

(define-for-syntax (define-for*/hash:-variant hash-maker)
  (lambda (stx)
    (syntax-parse stx
      #:literals (:)
      [(_ a1:optional-standalone-annotation*
          clause:for-clauses
          a2:optional-standalone-annotation*
          body ...) ; body is not always an expression, can be a break-clause
       (define a.ty (or (attribute a2.ty) (attribute a1.ty)))
       (quasisyntax/loc stx
         (for*/fold: #,@(if a.ty #`(: #,a.ty) #'())
             #,(if a.ty
                   #`((return-hash : #,a.ty (ann (#,hash-maker null) #,a.ty)))
                   #`((return-hash (#,hash-maker null))))
           (clause.expand* ... ...)
           (let-values (((key val) (let () body ...)))
             (hash-set return-hash key val))))])))

(define-syntax for*/hash:    (define-for*/hash:-variant #'make-immutable-hash))
(define-syntax for*/hasheq:  (define-for*/hash:-variant #'make-immutable-hasheq))
(define-syntax for*/hasheqv: (define-for*/hash:-variant #'make-immutable-hasheqv))


(define-syntax (provide: stx)
  (syntax-parse stx
    [(_ [i:id t] ...)
     ;; indirection through i*s allows `provide: to come
     ;; before the original definitions/type annotations
     (define i*s (generate-temporaries #'(i ...)))
     (for ([i* (in-list i*s)]
           [i  (in-list (syntax->list #'(i ...)))])
       ;; lift allows `provide:` to come before original definition
       (syntax-local-lift-module-end-declaration #`(define #,i* #,i)))
     (define/with-syntax (i* ...) i*s)
     (syntax/loc stx
       (begin (: i* t) ...
              (provide (rename-out [i* i] ...))))]))

(define-syntax (declare-refinement stx)
  (syntax-parse stx
    [(_ p:id)
     (quasisyntax/loc stx #,(internal #'(declare-refinement-internal p)))]))

(define-syntaxes (-let/cc -let/ec)
  (let ()
    (define-literal-syntax-class #:for-label Values)
    (define-literal-syntax-class #:for-label values)
    (define ((mk l/c) stx)
      (syntax-parse stx
       [(_ (~or (~var k (param-annotated-name
                         (λ (ty)
                            (syntax-parse ty
                              [((~or :Values^ :values^) tys ...) ;; binds types and ellipses
                               #'(tys ... -> (U))]
                              [t #'(t -> (U))]))))
                (~and k:id (~bind [k.ann-name #'k]))) . body)
        (quasisyntax/loc stx (#,l/c k.ann-name . body))]))
    (values (mk #'let/cc) (mk #'let/ec))))


;; lambda with optional type annotations, uses syntax properties
(define-syntax (-lambda stx)
  (syntax-parse stx
    #:literals (:)
    [(_ vars:maybe-lambda-type-vars
        formals:lambda-formals
        return:return-ann
        (~describe "body expression or definition" e) ...
        (~describe "body expression" last-e))
     ;; Annotate the last expression with the return type. Should be correct
     ;; since if a function returns, it has to do so through the last expression
     ;; even with continuations.
     (define/with-syntax last-e*
       (if (attribute return.type)
           #`(ann last-e #,(attribute return.type))
           #'last-e))
     (define d (syntax/loc stx (λ formals.erased e ... last-e*)))
     (define d/prop
       (if (attribute formals.kw-property)
           (kw-lambda-property d (attribute formals.kw-property))
           (opt-lambda-property d (attribute formals.opt-property))))
     ;; attach a plambda property if necessary
     (if (attribute vars.type-vars)
         (plambda-property d/prop (attribute vars.type-vars))
         d/prop)]))

;; for backwards compatibility, note that this only accepts formals
;; with type annotations and also accepts type variables differently
;; than -define
(define-syntax (define: stx)
  (syntax-parse stx #:literals (:)
    [(define: (nm:id . formals:annotated-formals)
       (~describe "return type annotation" (~seq : ret-ty)) body ...)
     (with-syntax ([arrty (syntax/loc stx (formals.arg-ty ... -> ret-ty))])
       (quasisyntax/loc stx
         (-define nm : arrty
           #,(syntax/loc stx (-lambda formals body ...)))))]
    [(define: nm:id ~! (~describe ":" :) (~describe "type" ty) body)
     (syntax/loc stx (-define nm : ty body))]
    [(define: tvars:type-variables nm:id : ty body)
     (syntax/loc stx (-define #:forall tvars nm : ty body))]
    [(define: tvars:type-variables (nm:id . formals:annotated-formals) : ret-ty body ...)
     (syntax/loc stx (-define #:forall tvars (nm . formals) : ret-ty body ...))]))

(define-syntax (-define stx)
  (syntax-parse stx #:literals (:)
    ;; the first three cases are actually subsumed by the last,
    ;; but manually expanding to using the : annotation form
    ;; produces better error messages on duplicate annotations
    [(-define nm:id body)
     (syntax/loc stx (define nm body))]
    [(-define nm:id return:return-ann body)
     (quasisyntax/loc stx
       (begin (: nm #,(attribute return.type)) (define nm body)))]
    [(-define vars:lambda-type-vars nm:id : ty body)
     (define/with-syntax type
       (syntax/loc #'ty (All vars.type-vars ty)))
     (syntax/loc stx
       (begin
         (: nm : type)
         (define nm body)))]
    [(-define (nm:id . formals:annotated-formals) : ret-ty body ...)
     (with-syntax ([arrty (syntax/loc stx (formals.arg-ty ... -> ret-ty))])
       (quasisyntax/loc stx
         (-define nm : arrty
           #,(syntax/loc stx (-lambda formals body ...)))))]
    ;; bug 14702: the below should generate a `:` annotation when possible
    ;; currently, the above special case does the right thing for non-curried lambdas
    [(-define vars:maybe-lambda-type-vars
              formals:curried-formals
              return:return-ann
              body ... last-body)
     ;; have to preprocess for the return type annotation
     (define/with-syntax last-body*
       (if (attribute return.type)
           #`(ann last-body #,(attribute return.type))
           #'last-body))
     (define-values (defined-id rhs)
       (normalize-definition
        #`(define formals.erased body ... last-body*)
        #'-lambda
        #t #t))
     ;; insert in type variables if necessary
     (define rhs*
       (syntax-parse rhs
         #:literals (-lambda)
         [(-lambda formals . others)
          (quasisyntax/loc stx (-lambda #,@(syntax vars) formals . others))]
         [_ rhs]))
     (quasisyntax/loc stx (define #,defined-id #,rhs*))]))

(define-syntax (with-asserts stx)
  (define-syntax-class with-asserts-clause
    [pattern [x:id]
             #:with cond-clause
             (syntax/loc #'x
               [(not x)
                (error "Assertion failed")])]
    [pattern [x:id pred]
             #:with cond-clause
             (syntax/loc #'x
               [(not (pred x))
                (error "Assertion failed")])])
   (syntax-parse stx
     [(_ (c:with-asserts-clause ...) body:expr ...+)
      (syntax-property
       (quasisyntax/loc stx
         (cond c.cond-clause
               ...
               [else #,(syntax-property
                        #'(begin body ...)
                        'feature-profile:TR-dynamic-check 'antimark)]))
       'feature-profile:TR-dynamic-check #t)]))

(define-syntax (typecheck-fail stx)
  (syntax-parse stx
    [(_ orig msg:str #:covered-id var:id)
     #'(quote (typecheck-fail-internal orig msg var))]
    [(_ orig msg:str)
     #'(quote (typecheck-fail-internal orig msg #f))]
    [(_ orig #:covered-id var:id)
     #'(quote (typecheck-fail-internal orig "Incomplete case coverage" var))]
    [(_ orig)
     #'(quote(typecheck-fail-internal orig "Incomplete case coverage" #f))]))

(define-syntax (base-for/vector stx)
  (syntax-case stx ()
    [(name for ann T K #:length n-expr #:fill fill-expr (clauses ...) body-expr)
     (syntax/loc stx
       (call/ec
        (ann (λ (break)
               (define n n-expr)
               (define vs (ann (make-vector n fill-expr) T))
               (define i 0)
               (for (clauses ...)
                 (unsafe-vector-set! vs i body-expr)
                 (set! i (unsafe-fx+ i 1))
                 (when (i . unsafe-fx>= . n) (break vs)))
               vs)
             K)))]
    [(name for ann T K #:length n-expr (clauses ...) body-expr)
     (syntax/loc stx
       (let ([n n-expr])
         (define vs
           (call/ec
            (ann (λ (break)
                   (define vs (ann (vector) T))
                   (define i 0)
                   (for (clauses ...)
                     (define v body-expr)
                     (cond [(unsafe-fx= i 0)  (define new-vs (ann (make-vector n v) T))
                                              (set! vs new-vs)]
                           [else  (unsafe-vector-set! vs i v)])
                     (set! i (unsafe-fx+ i 1))
                     (when (i . unsafe-fx>= . n) (break vs)))
                   vs)
                 K)))
         (cond [(= (vector-length vs) n)  vs]
               [else
                ;; Only happens when n > 0 and vs = (vector)
                (raise-result-error 'name (format "~e-element vector" n) vs)])))]
    [(_ for ann T K (clauses ...) body-expr)
     (syntax/loc stx
       (let ()
         (define n 0)
         (define vs (ann (vector) T))
         (define i 0)
         (for (clauses ...)
           (define v body-expr)
           (cond [(unsafe-fx= i n)  (define new-n (max 4 (unsafe-fx* 2 n)))
                                    (define new-vs (ann (make-vector new-n v) T))
                                    (vector-copy! new-vs 0 vs)
                                    (set! n new-n)
                                    (set! vs new-vs)]
                 [else  (unsafe-vector-set! vs i v)])
           (set! i (unsafe-fx+ i 1)))
         (cond 
          [(= i (vector-length vs)) vs]
          ;; We inline `vector-copy` to avoid a dependency.
          ;; The vector-ref here ensures that we have a well-typed initial element.
          [else (define new-vs (ann (make-vector i (vector-ref vs 0)) T))
                (vector-copy! new-vs 1 vs 1 i)
                new-vs])))]))

(define-for-syntax (base-for/vector: stx for:)
  (syntax-parse stx #:literals (:)
    [(name (~optional (~seq : T:expr))
           (~optional (~seq #:length n-expr:expr))
           (~optional (~seq #:fill fill-expr:expr))
           (clauses ...)
           (~optional (~seq : A:expr))
           body ...+)
     (let ([T  (attribute T)]
           [A  (attribute A)])
       (with-syntax ([(maybe-length ...)  (if (attribute n-expr) #'(#:length n-expr) #'())]
                     [(maybe-fill ...)  (if (attribute fill-expr) #'(#:fill fill-expr) #'())]
                     [body-expr  (if A #`(ann (let () body ...) #,A) #'(let () body ...))]
                     [T  (cond [(and T A)  #`(U #,T (Vectorof #,A))]
                               [T  T]
                               [A  #`(Vectorof #,A)]
                               [else  #'(Vectorof Any)])])
         (quasisyntax/loc stx
           (base-for/vector #,for: ann T ((T -> Nothing) -> T)
                            maybe-length ... maybe-fill ... (clauses ...) body-expr))))]))

(define-syntax (for/vector: stx)
  (base-for/vector: stx #'for:))

(define-syntax (for*/vector: stx)
  (base-for/vector: stx #'for*:))

(define-syntax (base-for/flvector: stx)
  (syntax-parse stx
    [(_ for: Float flvector make-flvector unsafe-flvector-ref unsafe-flvector-set! flvector-copy
        #:length n-expr:expr (clauses ...) body ...+)
     (syntax/loc stx
       (-let ([n : Integer  n-expr])
         (cond [(n . > . 0)
                (define xs (make-flvector n))
                (define: i : Nonnegative-Fixnum 0)
                (-let/ec break : Void
                  (for: (clauses ...)
                    (unsafe-flvector-set! xs i (let () body ...))
                    (set! i (unsafe-fx+ i 1))
                    (when (i . unsafe-fx>= . n) (break (void)))))
                xs]
               [else  (flvector)])))]
    [(_ for: Float flvector make-flvector unsafe-flvector-ref unsafe-flvector-set! flvector-copy
        (clauses ...) body ...+)
     (syntax/loc stx
       (let ()
         (define n 4)
         (define xs (make-flvector 4))
         (define i 0)
         (for: (clauses ...)
           (-let ([x : Float  (let () body ...)])
             (cond [(unsafe-fx= i n)  (define new-n (unsafe-fx* 2 n))
                                      (define new-xs (make-flvector new-n x))
                                      (-let loop : Void ([i : Nonnegative-Fixnum 0])
                                        (when (i . unsafe-fx< . n)
                                          (unsafe-flvector-set! new-xs i (unsafe-flvector-ref xs i))
                                          (loop (unsafe-fx+ i 1))))
                                      (set! n new-n)
                                      (set! xs new-xs)]
                   [else  (unsafe-flvector-set! xs i x)]))
           (set! i (unsafe-fx+ i 1)))
         (flvector-copy xs 0 i)))]))

(define-syntax-rule (for/flvector: e ...)
  (base-for/flvector: for: Flonum flvector make-flvector
                      unsafe-flvector-ref unsafe-flvector-set! flvector-copy e ...))

(define-syntax-rule (for*/flvector: e ...)
  (base-for/flvector: for*: Flonum flvector make-flvector
                      unsafe-flvector-ref unsafe-flvector-set! flvector-copy e ...))

(define-syntax-rule (for/extflvector: e ...)
  (base-for/flvector: for: ExtFlonum extflvector make-extflvector unsafe-extflvector-ref
                      unsafe-extflvector-set! extflvector-copy e ...))

(define-syntax-rule (for*/extflvector: e ...)
  (base-for/flvector: for*: ExtFlonum extflvector make-extflvector unsafe-extflvector-ref
                      unsafe-extflvector-set! extflvector-copy e ...))
