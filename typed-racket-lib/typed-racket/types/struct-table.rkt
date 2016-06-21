#lang racket/base

(require racket/dict syntax/id-table racket/match
         racket/syntax
         "../utils/utils.rkt"
         (prefix-in c: (contract-req))
         (rep type-rep prop-rep object-rep)
         (utils tc-utils)
         (env env-utils)
         (types abbrev))

(define struct-fn-table (make-free-id-table))

(define (add-struct-fn! id pe mut?) (dict-set! struct-fn-table id (list pe mut?)))

(define-values (struct-accessor? struct-mutator?)
  (let ()
    (define ((mk mut?) id)
      (cond [(dict-ref struct-fn-table id #f)
             => (match-lambda [(list pe m) (and (eq? m mut?) pe)] [_ #f])]
            [else #f]))
    (values (mk #f) (mk #t))))

(define (struct-fn-idx id)
  (match (dict-ref struct-fn-table id #f)
    [(list (StructPE: _ idx) _) idx]
    [_ (int-err (format "no struct fn table entry for ~a" (syntax->datum id)))]))

(define (struct-fn-table-map f)
  (for/list ([(k v) (in-sorted-dict struct-fn-table id<)])
    (f k v)))

(provide/cond-contract
 [add-struct-fn! (identifier? StructPE? boolean? . c:-> . c:any/c)]
 [struct-accessor? (identifier? . c:-> . (c:or/c #f StructPE?))]
 [struct-mutator? (identifier? . c:-> . (c:or/c #f StructPE?))]
 [struct-fn-idx (identifier? . c:-> . exact-integer?)]
 [struct-fn-table-map (c:-> (c:-> identifier? (c:list/c StructPE? boolean?) c:any/c)
                            (c:listof/c c:any/c))])
