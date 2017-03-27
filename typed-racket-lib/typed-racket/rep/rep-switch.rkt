#lang racket/base

(require "rep-utils.rkt"
         racket/match
         racket/unsafe/ops
         (for-syntax racket/base
                     syntax/parse
                     racket/list
                     racket/syntax))

(provide define-rep-switch)


;; a macro for defining a switch function of the form:
;; (define name (λ (a b ...) (switch (Rep-uid a) [switch-clauses ...])))
;;
;; This allows us to dispatch on the first arguments Rep-uid,
;; which can be more efficient than long match statements with a case
;; for large numbers of Reps (e.g. subtype)
(define-syntax (define-rep-switch stx)
  (define-syntax-class (switch-clause pre-args arg post-args)
    (pattern (((~datum case:) rep-name:id pattern:expr) . body)
             #:with name #'rep-name
             #:with idx (format-id #'rep-name "uid:~a" (syntax->datum #'rep-name))
             #:with function
             (with-syntax ([(pre-args ...) pre-args]
                           [arg arg]
                           [(post-args ...) post-args])
               (syntax/loc #'body
                 (λ (pre-args ... arg post-args ...)
                   (match arg
                     [pattern . body]))))))
  (syntax-parse stx
    [(_ (name:id pre-args:id ... (#:switch arg:id) post-args:id ...)
        (~var clause (switch-clause #'(pre-args ...) #'arg #'(post-args ...))) ...
        [(~datum else:) . default])
     (define name-symbols (map syntax->datum (syntax->list #'(clause.name ...))))
     (unless (not (null? name-symbols))
       (raise-syntax-error 'define-switch "switch cannot be null" stx))
     (define sorted-name-symbols (sort name-symbols symbol<?))
     (unless (eq? (first name-symbols) (first sorted-name-symbols))
       (raise-syntax-error 'define-switch
                           (format "expected ~a as the first case"
                                   (first sorted-name-symbols))
                           stx))
     ;; we verify that the Rep cases appear in name-sorted order (so
     ;; they are easier to find when browsing the code) and that there
     ;; are no duplicate cases
     (for ([cur (in-list (rest name-symbols))]
           [cur-stx (in-list (rest (syntax->list #'(clause ...))))]
           [cur* (in-list (rest sorted-name-symbols))]
           [prev* (in-list sorted-name-symbols)]
           [prev-stx (in-list (syntax->list #'(clause ...)))])
       (when (eq? cur* prev*)
         (raise-syntax-error 'define-switch
                             (format "duplicate switch cases for ~a" prev*)
                             prev-stx))
       (unless (eq? cur cur*)
         (raise-syntax-error 'define-switch
                             (format "switch cases must be sorted! expected ~a but got ~a"
                                     cur* cur)
                             cur-stx)))
     (syntax/loc stx
       (define name
         (let* ([default-fun (λ (pre-args ... arg post-args ...) . default)]
                [switch-table (make-vector (get-uid-count) default-fun)])
           (vector-set! switch-table clause.idx clause.function)
           ...
           (λ (pre-args ... arg post-args ...)
             ((unsafe-vector-ref switch-table (Rep-uid arg)) pre-args ... arg post-args ...)))))]))
