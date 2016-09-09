#lang racket/base

;; Support for defining the initial TR environment

(require "../utils/utils.rkt"
         "../utils/tc-utils.rkt"
         "global-env.rkt"
         "type-name-env.rkt"
         "type-alias-env.rkt"
         "mvar-env.rkt"
         "signature-env.rkt"
         (rename-in racket/private/sort [sort raw-sort])
         (rep core-rep type-rep
              prop-rep rep-utils
              object-rep values-rep
              free-variance)
         (for-syntax syntax/parse racket/base)
         (types abbrev struct-table union utils)
         data/queue
         racket/dict racket/list racket/set racket/promise
         racket/match)

(provide ;; convenience form for defining an initial environment
         ;; used by "base-special-env.rkt" and "base-contracted.rkt"
         define-initial-env
         initialize-type-name-env
         initialize-type-env
         type->sexp ; for types/printer.rkt
         make-env-init-codes)

(define-syntax (define-initial-env stx)
  (syntax-parse stx
    [(_ initialize-env [id-expr ty] ...)
     #`(begin
         (define initial-env (make-env [id-expr (λ () ty)] ... ))
         (define (initialize-env) (initialize-type-env initial-env))
         (provide initialize-env))]))

(define (initialize-type-name-env initial-type-names)
  (for-each (lambda (nm/ty) (register-resolved-type-alias (car nm/ty) (cadr nm/ty))) initial-type-names))

(define (initialize-type-env initial-env)
  (for-each (lambda (nm/ty) (register-type-if-undefined (car nm/ty) (cadr nm/ty))) initial-env))

;; stores definition syntaxes for lifting out common expressions
(define type-definitions (make-queue))

;; -> Syntax
;; Emit stored type definitions as code to run before env code
(define (get-extra-type-definitions)
  #`(begin #,@(queue->list type-definitions)))

;; cache for memoizing the type->sexp computation
(define type-cache (make-hash))

;; (HashTable Type Natural)
;; Keep track of the popularities of types
(define pop-table (make-hash))

;; Compute for a given type how many times each type inside of it
;; is referenced
(define (compute-popularity ty)
  (when (Type? ty)
    (hash-update! pop-table ty add1 0))
  (when (walkable? ty)
    (Rep-walk compute-popularity ty)))

(define (popular? ty)
  (> (hash-ref pop-table ty 0) 5))

;; Type -> S-Exp
;; Convert a type to an s-expression to evaluate
(define (type->sexp ty)
  (cond [(hash-ref type-cache ty #f)]
        [else
         (define *res (recur ty))
         (define res
           (cond ;; lift type out as a definition if it's referenced enough
                 ;; and also isn't just a identifier already due to the
                 ;; predefined table
                 [(and (not (identifier? *res))
                       (popular? ty))
                  (define id (gensym))
                  (enqueue! type-definitions #`(define #,id #,*res))
                  id]
                 [else *res]))
        (hash-set! type-cache ty res)
        res]))

(define-match-expander In-Predefined-Table:
  (λ (stx)
    (syntax-parse stx
      [(_ id)
       #'(? Rep? (app (λ (v) (hash-ref predefined-type-table (Rep-seq v) #f))
                      (? values id)))])))

;; Helper for type->sexp
(define (recur ty)
  (define (numeric? t) (match t [(Base: _ _ _ b) b] [(Value: (? number?)) #t] [_ #f]))
  (define (split-union ts)
    (define-values (nums others) (partition numeric? ts))
    (cond [(or (null? nums) (null? others))
            ;; nothing interesting to do in this case
            `(make-Union (list ,@(map type->sexp ts)))]
           [else
            ;; we do a little more work to hopefully save a bunch in serialization space
            ;; if we get a hit in the predefined-type-table
            `(simple-Un ,(type->sexp (apply Un nums))
                        ,(type->sexp (apply Un others)))]))

  (match ty
    [(In-Predefined-Table: id) id]
    [(Base: n cnt pred _)
     (int-err "Base type ~a not in predefined-type-table" n)]
    [(B: nat) `(make-B ,nat)]
    [(F: sym) `(make-F (quote ,sym))]
    [(Pair: ty (Listof: ty))
     `(-ne-lst ,(type->sexp ty))]
    [(Pair: left right)
     `(make-Pair ,(type->sexp left) ,(type->sexp right))]
    [(ListDots: type dbound)
     `(make-ListDots ,(type->sexp type) (quote ,dbound))]
    [(MPair: left right)
     `(make-MPair ,(type->sexp left) ,(type->sexp right))]
    [(Vector: ty)
     `(make-Vector ,(type->sexp ty))]
    [(HeterogeneousVector: elems)
     `(-vec* ,@(map type->sexp elems))]
    [(Box: ty)
     `(make-Box ,(type->sexp ty))]
    [(Channel: ty)
     `(make-Channel ,(type->sexp ty))]
    [(Async-Channel: ty)
     `(make-Async-Channel ,(type->sexp ty))]
    [(ThreadCell: ty)
     `(make-ThreadCell ,(type->sexp ty))]
    [(Promise: ty)
     `(make-Promise ,(type->sexp ty))]
    [(Ephemeron: ty)
     `(make-Ephemeron ,(type->sexp ty))]
    [(Weak-Box: ty)
     `(make-Weak-Box ,(type->sexp ty))]
    [(CustodianBox: ty)
     `(make-CustodianBox ,(type->sexp ty))]
    [(Set: ty)
     `(make-Set ,(type->sexp ty))]
    [(Evt: ty)
     `(make-Evt ,(type->sexp ty))]
    [(Future: ty)
     `(make-Future ,(type->sexp ty))]
    [(Prompt-Tagof: prompt handler)
     `(make-Prompt-Tagof ,(type->sexp prompt) ,(type->sexp handler))]
    [(Continuation-Mark-Keyof: ty)
     `(make-Continuation-Mark-Keyof ,(type->sexp ty))]
    [(Sequence: tys)
     `(-seq ,@(map type->sexp tys))]
    [(Syntax: ty)
     `(make-Syntax ,(type->sexp ty))]
    [(Listof: elem-ty)
     `(-lst ,(type->sexp elem-ty))]
    [(Param: ty ty)
     `(-Param ,(type->sexp ty))]
    [(Param: in out)
     `(make-Param ,(type->sexp in) ,(type->sexp out))]
    [(Hashtable: key val)
     `(make-Hashtable ,(type->sexp key) ,(type->sexp val))]
    [(Function: (list (arr: dom (Values: (list (Result: t
                                                        (PropSet: (TrueProp:)
                                                                  (TrueProp:))
                                                        (Empty:))))
                            #f #f '())))
     `(simple-> (list ,@(map type->sexp dom)) ,(type->sexp t))]
    [(Function: (list (arr: dom (Values: (list (Result: t (PropSet: (TypeProp: pth ft)
                                                                    (NotTypeProp: pth ft))
                                                        (Empty:))))
                            #f #f '())))
     `(make-pred-ty (list ,@(map type->sexp dom))
                    ,(type->sexp t)
                    ,(type->sexp ft)
                    ,(object->sexp pth))]
    [(Function: (list (arr: dom (Values: (list (Result: t (PropSet: (NotTypeProp: (Path: pth (cons 0 0))
                                                                                  (== -False))
                                                                    (TypeProp: (Path: pth (cons 0 0))
                                                                               (== -False)))
                                                        (Path: pth (cons 0 0)))))
                            #f #f '())))
     `(->acc (list ,@(map type->sexp dom))
             ,(type->sexp t)
             (list ,@(map path-elem->sexp pth)))]
    [(Function: (? has-optional-args? arrs))
     (match-define (arr: fdoms rng rest _ *kws) (first arrs))
     (match-define (arr: ldoms _ _ _ _) (last arrs))
     (define opts (drop ldoms (length fdoms)))
     (define kws (map type->sexp *kws))
     `(opt-fn (list ,@(map type->sexp fdoms))
              (list ,@(map type->sexp opts))
              ,(type->sexp rng)
              ,@(if rest `(#:rest ,rest) '())
              ,@(if (null? kws) '() `(#:kws (list ,@kws))))]
    [(Function: arrs)
     `(make-Function (list ,@(map type->sexp arrs)))]
    [(Keyword: kw ty required?)
     `(make-Keyword (quote ,kw) ,(type->sexp ty) ,required?)]
    [(Values: rs)
     `(make-Values (list ,@(map type->sexp rs)))]
    [(ValuesDots: rs dty dbound)
     `(make-ValuesDots (list ,@(map type->sexp rs))
                       ,(type->sexp dty)
                       (quote ,dbound))]
    [(Result: t (PropSet: (TrueProp:) (TrueProp:)) (Empty:))
     `(-result ,(type->sexp t))]
    [(Result: ty prop obj)
     `(make-Result ,(type->sexp ty)
                   ,(prop->sexp prop)
                   ,(object->sexp obj))]
    [(AnyValues: prop)
     `(make-AnyValues ,(prop->sexp prop))]
    [(Union: (list (Value: vs) ...))
     `(one-of/c ,@(for/list ([v (in-list vs)])
                    `(quote ,v)))]
    [(Union: elems) (split-union elems)]
    [(Intersection: elems)
     `(make-Intersection (list ,@(map type->sexp elems)))]
    [(Name: stx 0 #t)
     `(-struct-name (quote-syntax ,stx))]
    [(Name: stx args struct?)
     `(make-Name (quote-syntax ,stx) ,args ,struct?)]
    [(fld: t acc mut)
     `(make-fld ,(type->sexp t) (quote-syntax ,acc) ,mut)]
    [(Struct: name parent flds proc poly? pred-id)
     `(make-Struct (quote-syntax ,name)
                   ,(and parent (type->sexp parent))
                   (list ,@(map type->sexp flds))
                   ,(and proc (type->sexp proc))
                   ,poly?
                   (quote-syntax ,pred-id))]
    [(StructType: struct) `(make-StructType ,(type->sexp struct))]
    [(Prefab: key flds)
     `(make-Prefab (quote ,key)
                   (list ,@(map type->sexp flds)))]
    [(App: rator rands stx)
     `(make-App ,(type->sexp rator)
                (list ,@(map type->sexp rands))
                ,(and stx `(quote-syntax ,stx)))]
    [(Opaque: pred)
     `(make-Opaque (quote-syntax ,pred))]
    [(Refinement: parent pred)
     `(make-Refinement ,(type->sexp parent) (quote-syntax ,pred))]
    [(Mu-name: n b)
     `(make-Mu (quote ,n) ,(type->sexp b))]
    [(Poly-names: ns b)
     `(make-Poly (list ,@(for/list ([n (in-list ns)])
                           `(quote ,n)))
                 ,(type->sexp b))]
    [(PolyDots-names: ns b)
     `(make-PolyDots (list ,@(for/list ([n (in-list ns)])
                           `(quote ,n)))
                     ,(type->sexp b))]
    [(PolyRow-names: ns c b)
     `(make-PolyRow (list ,@(for/list ([n (in-list ns)])
                              `(quote ,n)))
                    (quote ,c)
                    ,(type->sexp b))]
    [(Row: inits fields methods augments init-rest)
     `(make-Row (list ,@(convert-row-clause inits #t))
                (list ,@(convert-row-clause fields))
                (list ,@(convert-row-clause methods))
                (list ,@(convert-row-clause augments))
                ,(and init-rest (type->sexp init-rest)))]
    [(Class: row inits fields methods augments init-rest)
     `(make-Class ,(and row (type->sexp row))
                  (list ,@(convert-row-clause inits #t))
                  (list ,@(convert-row-clause fields))
                  (list ,@(convert-row-clause methods))
                  (list ,@(convert-row-clause augments))
                  ,(and init-rest (type->sexp init-rest)))]
    [(Instance: ty) `(make-Instance ,(type->sexp ty))]
    [(Signature: name extends mapping)
     (define (serialize-mapping m)
       (map (lambda (id/ty) 
              (define id (car id/ty))
              (define ty (force (cdr id/ty)))
              `(cons (quote-syntax ,id) ,(type->sexp ty)))
            m))
     (define serialized-extends (and extends `(quote-syntax ,extends)))
     `(make-Signature (quote-syntax ,name)
                      ,serialized-extends
                      (list ,@(serialize-mapping mapping)))]
    [(Unit: imports exports init-depends result)
     `(make-Unit (list ,@(map type->sexp imports))
                 (list ,@(map type->sexp exports))
                 (list ,@(map type->sexp init-depends))
                 ,(type->sexp result))]
    [(arr: dom (Values: (list (Result: t (PropSet: (TrueProp:)
                                                   (TrueProp:))
                                       (Empty:))))
           #f #f '())
     `(make-arr* (list ,@(map type->sexp dom))
                 ,(type->sexp t))]
    [(arr: dom rng #f #f '())
     `(make-arr* (list ,@(map type->sexp dom))
                 ,(type->sexp rng))]
    [(arr: dom rng rest drest kws)
     `(make-arr (list ,@(map type->sexp dom))
                ,(type->sexp rng)
                ,(and rest (type->sexp rest))
                ,(and drest `(cons ,(type->sexp (car drest))
                                   (quote ,(cdr drest))))
                (list ,@(map type->sexp kws)))]
    [(Distinction: nm id ty)
     `(make-Distinction (quote ,nm)
                        (quote ,id)
                        ,(type->sexp ty))]
    [(Value: v) `(make-Value (quote ,v))]
    ;; Most Top types are in the predefined table, the ones here
    ;; are not
    [(StructTop: name) `(make-StructTop ,(type->sexp name))]))

;; Helper for class/row clauses
(define (convert-row-clause members [inits? #f])
  (for/list ([m (in-list members)])
    `(list (quote ,(car m))
           ,(type->sexp (cadr m))
           ,@(if inits? (cddr m) '()))))

;; Prop -> Sexp
;; Convert a prop to an s-expression
(define (prop->sexp prop)
  (match prop
    [(In-Predefined-Table: id) id]
    ;; TrueProp/FalseProp are predefined
    [(TypeProp: o t)
     `(make-TypeProp ,(object->sexp o) ,(type->sexp t))]
    [(NotTypeProp: o t)
     `(make-NotTypeProp ,(object->sexp o) ,(type->sexp t))]
    [(AndProp: fs)
     `(make-AndProp (list ,@(map prop->sexp fs)))]
    [(OrProp: fs)
     `(make-OrProp (list ,@(map prop->sexp fs)))]
    [(PropSet: thn els)
     `(make-PropSet ,(prop->sexp thn) ,(prop->sexp els))]))

;; Object -> SExp
;; Convert an object to an s-expression to eval
(define (object->sexp obj)
  (match obj
    [(Empty:) `(make-Empty)]
    [(Path: null (cons 0 arg))
     `(-arg-path ,arg)]
    [(Path: null (cons depth arg))
     `(-arg-path ,arg ,depth)]
    [(Path: pes i)
     `(make-Path (list ,@(map path-elem->sexp pes))
                 ,(if (identifier? i)
                      `(quote-syntax ,i)
                      `(cons ,(car i) ,(cdr i))))]))

;; Path-Element -> SExp
;; Convert a path element in an object to an s-expression
(define (path-elem->sexp pe)
  (match pe
    [(In-Predefined-Table: id) id]
    ;; CarPE, CdrPE, SyntaxPE, ForcePE, FieldPE are in the table
    [(StructPE: ty idx)
     `(make-StructPE ,(type->sexp ty) ,idx)]))

(define (bound-in-this-module id)
  (let ([binding (identifier-binding id)])
    (if (and (list? binding) (module-path-index? (car binding)))
        (let-values ([(mp base) (module-path-index-split (car binding))])
          (not mp))
        #f)))

(define (make-init-code map f)
  (define (bound-f id v)
    (and (bound-in-this-module id) (f id v)))
  (define aliases (filter values (map bound-f)))
  #`(begin #,@aliases))

(define (quote-type ty)
  (datum->syntax #'here (type->sexp ty)))

;; -> Void
;; Populates the table of type reference counts in order to inform
;; the type serialization pass. Only walks the environments that
;; actually track types.
(define (compute-all-popularities)
  (define (count-env for-each)
    (define (count id ty) (compute-popularity ty))
    (define (bound-f id v)
      (and (bound-in-this-module id) (count id v)))
    (for-each bound-f))

  (count-env type-name-env-for-each)
  (count-env type-alias-env-for-each)
  (count-env type-env-for-each)
  (count-env signature-env-for-each))

(define (tname-env-init-code)
  (make-init-code
    type-name-env-map
    (λ (id ty) #`(register-type-name #'#,id #,(quote-type ty)))))

(define (tvariance-env-init-code)
  (make-init-code
    type-variance-env-map
    (λ (id var) #`(register-type-variance! #'#,id (list #,@(map variance->binding var))))))

(define (talias-env-init-code)
  (make-init-code
    type-alias-env-map
    (λ (id ty) #`(register-resolved-type-alias #'#,id #,(quote-type ty)))))

(define (env-init-code)
  (make-init-code
    type-env-map
    (λ (id ty) #`(register-type #'#,id #,(quote-type ty)))))

(define (mvar-env-init-code mvar-env)
  (make-init-code
    (λ (f) (dict-map mvar-env f))
    (lambda (id v) (and v #`(register-mutated-var #'#,id)))))

(define (signature-env-init-code)
  (make-init-code
   signature-env-map
   (lambda (id sig) #`(register-signature! #'#,id (delay #,(quote-type sig))))))

(define (make-struct-table-code)
  (make-init-code
   struct-fn-table-map
   (λ (id v)
     (match-define (list pe mut?) v)
     #`(add-struct-fn! (quote-syntax #,id)
                       #,(path-elem->sexp pe)
                       #,mut?))))

;; -> (Listof Syntax)
;; Construct syntax that does type environment serialization
(define (make-env-init-codes)
  (compute-all-popularities)

  (define *env-codes
    (list (env-init-code)
          (talias-env-init-code)
          (tname-env-init-code)
          (tvariance-env-init-code)
          (mvar-env-init-code mvar-env)
          (signature-env-init-code)
          (make-struct-table-code)))

  ;; get the lifted common expressions for types which need to come first
  (list* (get-extra-type-definitions)
         *env-codes))
