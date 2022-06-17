#lang racket/base
(require racket/match
         racket/lazy-require
         racket/list
         racket/string
         racket/generic)

(provide print-kind
         make-type-constr
         type-constr-productive?
         type-constr-arity
         (struct-out TypeConstructor)
         (struct-out TypeConstructorStub)
         (struct-out exn:fail:contract:arity:type-constructor))

(define-values (prop:kind kind? kind-acc) (make-struct-type-property 'kind))
(provide prop:kind kind?)


(struct exn:fail:contract:arity:type-constructor exn:fail:contract:arity (expected given) #:transparent
  #:constructor-name make-exn:fail:contract:arity:type-constructor)


(provide gen:type-rep-maker gen-serialize-type-rep)

(define-generics type-rep-maker
  #:fast-defaults
  ([procedure?
    (define (gen-create-type-rep proc args)
      (apply proc args))
    (define (gen-serialize-type-rep proc ty->sexp)
      (object-name proc))])
  [gen-create-type-rep type-rep-maker args]
  [gen-serialize-type-rep type-rep-maker ty->sexp])


(struct TypeConstructorBase (arity kind*? productive?) #:transparent)

(define (type-constr-productive? ty-op)
  (TypeConstructorBase-productive? ty-op))

(define (type-constr-arity ty-op)
  (TypeConstructorBase-arity ty-op))

;; real-trep-constr: the underlying *named* type rep constructor
;; arity: the mandatory arity
;; kind*: whether this type constructor can take an arbitrary number of arguments
;; productive?: whether this type constructor is productive.
(struct TypeConstructor TypeConstructorBase (real-trep-constr)
  #:transparent
  #:property prop:kind #t
  #:property prop:procedure
  (lambda (me . args)
    (match-define (TypeConstructor arity kind*? _ real-trep-constr) me)
    ;; FIXME: real-trep-constr can take other arguments than types.
    ;; This could make handling k* more complicated.
    ;; naive assumpution: type arguments come first in args.
    (define-values (type-args non-type-args) (splitf-at args kind?))
    (let ([len (length type-args)])
      (when (or (< len arity)
                (and (> len arity) (not kind*?)))
        (raise (make-exn:fail:contract:arity:type-constructor
                (format "number of expected type arguments: ~a~n given: ~a" arity len)
                (current-continuation-marks)
                arity len))))
    (gen-create-type-rep real-trep-constr
                         (cond
                           [kind*?
                            (define-values (mandatory-args seq-args) (split-at type-args arity))
                            (append mandatory-args (list seq-args) non-type-args)]
                           [else args]))))

(define (make-type-constr type-maker [arity 1] [productive #t] #:kind*? [kind*? #f])
  (when (and (procedure? type-maker) (not (object-name type-maker)))
    (error 'make-type-constr "only named procedures are allowed"))

  (if (and (zero? arity) (not kind*?))
      type-maker
      (TypeConstructor arity kind*? productive type-maker)))


(struct TypeConstructorStub TypeConstructorBase [] #:transparent)

(define (print-kind type-or-type-op)
  (match type-or-type-op
    [(struct* TypeConstructorBase ([arity arity]
                                   [kind*? kind*?]
                                   [productive? productive?]))
     (define mandatory-stars (make-list arity "*"))
     (define all-stars (if kind*? (append mandatory-stars (list "* ..."))
                           mandatory-stars))
     (format "(~a ~a *)" (if productive? "->" "-o")
             (string-join all-stars))]
    [_ "*"]))
