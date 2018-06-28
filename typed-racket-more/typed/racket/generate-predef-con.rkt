#lang racket/base
(require (for-syntax
          typed-racket/private/type-contract
          racket/base racket/match
          racket/syntax
          typed-racket/private/parse-type
          typed-racket/base-env/base-types-extra
          typed-racket/env/env-req)
         typed-racket/base-env/base-types-extra)
;; generate a good symbol name for this contract
;; Syntax -> Symbol
(define-for-syntax (->name stx)
  (syntax-case stx (Instance)
    [(Instance i) (format-symbol "~a-instance/c" #'i)]
    [i (identifier? #'i) (syntax-e #'i)]
    [_ 'gui_base/c]))


(provide generate-contract-submods)

(define-syntax (generate-contract-submods stx)
  (do-requires)
  (do-contract-requires)
  (define cache (make-hash))
  (define sc-cache (make-hash))
  (define sets-stx null)
  (define defs-stx null)
  (define (generate-contracts ty-name kind [side 'typed])
    (define typ (parse-type ty-name))
    (define name (->name ty-name))
    (match-define (list defs ctc)
       (type->contract
        typ
        #:typed-side (from-typed? side)
        #:kind kind
        #:cache (make-hash)
        #:sc-cache (make-hash)
        (lambda _ (error 'fail))))
    (define n (datum->syntax #'here (syntax-e (generate-temporary name))))
    (set! defs-stx (append defs-stx defs 
                           (list 
                            #`(provide #,n)
                            #`(define #,n #,ctc))))
    (set! sets-stx (cons #`(hash-set! predef-contracts
                                      (cons (parse-type #'#,ty-name) '#,side) 
                                      #'#,n)
                         sets-stx)))
  (syntax-case stx ()
    [(_ [ty kind (modes ...)] ...)
     (for ([ty (syntax->list #'(ty ...))]
           [kind (syntax->list #'(kind ...))]
           [modes (syntax->list #'((modes ...) ...))])
       (for ([mode (syntax->list modes)])
         (generate-contracts ty (syntax-e kind) (syntax-e mode))))])

  #`(begin
      (module* #%contract-defs #f
        (#%plain-module-begin 
         #,@(get-contract-requires)
         #,@defs-stx))
      (begin-for-syntax
        (module* #%contract-defs-names #f
          (require #,(syntax-local-introduce 
                      #'(submod typed-racket/static-contracts/instantiate
                                predefined-contracts))
                   typed-racket/private/parse-type
                   typed-racket/base-env/base-types-extra
                   #,(syntax-local-introduce #'(for-template (submod ".." #%contract-defs))))
          #,@(map syntax-local-introduce sets-stx)))))
