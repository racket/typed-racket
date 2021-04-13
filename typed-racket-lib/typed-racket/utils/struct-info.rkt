#lang racket/base

(require racket/struct-info
         racket/match
         "utils.rkt"
         (prefix-in c: (contract-req))
         "disarm.rkt"
         "../base-env/type-name-error.rkt")

(provide extract-struct-info/checked
         extract-struct-info/checked/context
         validate-struct-fields
         make-struct-info-wrapper*
         maybe-struct-info-wrapper-type
         struct-info->syntax)

(define (extract-struct-info/checked id)
  (extract-struct-info/checked/context id 'require/typed #f))

(define (extract-struct-info/checked/context id who ctx)
  (syntax-case id ()
   [id
    (and (identifier? #'id)
         (struct-info? (syntax-local-value #'id (lambda () #f))))
    (extract-struct-info (syntax-local-value #'id))]
   [_n
    (raise-syntax-error
     who
     "expected an identifier bound to a structure type" ctx id)]))

(define (validate-struct-fields name fields all-selectors who ctx)
  (define sel-names
    ;; filter super selectors
    (let* ([name-str (regexp-quote (format "~a" (syntax-e name)))]
           [sel-rx (regexp (string-append "^" name-str "-(.*)$"))])
      (filter
        values
        (for/list ([sel-id (in-list all-selectors)])
          (define m (and sel-id (regexp-match sel-rx (format "~a" (syntax-e sel-id)))))
          (and m (cadr m))))))
  ;; check number of fields
  (let ([num-fields (length fields)]
        [num-sels (length sel-names)])
    (define missing-selectors?
      ;; selector names might be mangled, e.g. by contract-out
      (< num-sels (length all-selectors)))
    (unless (or missing-selectors?
                (= num-fields num-sels))
      (raise-syntax-error
        who
        (format "found ~a field~a in type, but ~a field~a in struct ~a"
                num-fields (if (= 1 num-fields) "" "s")
                num-sels (if (= 1 num-sels) "" "s") (syntax-e name))
        ctx)))
  ;; check field names
  (for ([field-id (in-list fields)]
        [expected-name (in-list sel-names)]
        [i (in-naturals)])
    (define field-name (format "~a" (syntax-e field-id)))
    (unless (string=? field-name expected-name)
      (raise-syntax-error
        who
        (format "expected field name ~a to be ~a, but found ~a"
                i expected-name field-name)
        ctx field-id)))
  (void))

;Copied from racket/private/define-struct
;FIXME when multiple bindings are supported
(define (self-ctor-transformer orig stx)
  (define (transfer-srcloc orig stx)
    (datum->syntax (disarm* orig) (syntax-e orig) stx orig))

  (syntax-case stx ()
    [(self arg ...) (datum->syntax stx
                                   (cons (syntax-property (transfer-srcloc orig #'self)
                                                          'constructor-for
                                                          (syntax-local-introduce #'self))
                                         (syntax-e (syntax (arg ...))))
                                   stx
                                   stx)]
    [_ (transfer-srcloc orig stx)]))

;; struct-info-wrapper is used when a structure's name serves one sole purpose:
;; it will only be used in where a structure id is expected, e.g. struct-copy,
;; super-id in structures' definitions.

;; id -- struct name
;; info -- an struct-info instance for the structure
;; type -- the type name of the struct
(struct struct-info-wrapper (id info type)
  #:property prop:procedure
  (lambda (ins stx)
    (raise-syntax-error #f "identifier for static struct-type information cannot be used as an expression" stx))
  #:property prop:struct-info
  (λ (x)
    (extract-struct-info (struct-info-wrapper-info x))))

;; struct-info+type+self-ctor-wrapper is used when a structure's name works as
;; the constructor
(struct struct-info+type+self-ctor-wrapper struct-info-wrapper ()
  #:property prop:procedure
  (lambda (ins stx)
    (self-ctor-transformer (struct-info-wrapper-id ins) stx)))

;; when the argument is not an instance of any wrapper defined in this module,
;; i.e. it's a struct-info of a built-in structure,
;; the function returns the corresponding structure's type name
(define/cond-contract (maybe-struct-info-wrapper-type ins)
  (c:-> c:any/c (c:or/c #f identifier?))
  (if (struct-info-wrapper? ins)
      (struct-info-wrapper-type ins)
      #f))

;; create a *-wrapper instance based on sname-is-constr?
(define/cond-contract (make-struct-info-wrapper* id info type [sname-is-constr? #t])
  (c:->* (identifier? struct-info? identifier?) (boolean?) struct-info-wrapper?)
  (define s-ctor
    (cond
      [(and (not sname-is-constr?)) struct-info-wrapper]
      [else struct-info+type+self-ctor-wrapper]))
  (s-ctor id info type))


(define (struct-info->syntax si)
  (define (conv i)
    (if (identifier? i) #`(quote-syntax #,i)
        (quasisyntax #,i)))

  (match-define (list type-desc constr pred (list accs ...) (list muts ...) super) (extract-struct-info si))

  #`(list
     (syntax #,type-desc)
     (syntax #,constr)
     (syntax #,pred)
     (list #,@(map conv accs))
     (list #,@(map conv muts))
     #,(conv super)))
