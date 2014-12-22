#lang racket/base

;; implied-atomic? was improved for disjunctions
;; these tests make sure those improvements
;; are still working (these used to take
;; an astronomical amount of time)

(require racket/sandbox)

(call-with-limits
 120
 500
 (λ () (eval '(begin (module a typed/racket
                       (provide foo bar)
                       (: foo : Any -> Number)
                       (define (foo x)
                         (match x
                           [(cons (or 'x 'y) more)
                            (match x
                              [(list _ (list (list (? number? x) y (? number? xs) ys))) 1]
                              [_ 2])]))
                       
                       (: filters-more (-> Boolean Boolean Boolean Boolean Boolean 
                                           Boolean Boolean Boolean Boolean Natural))
                       (define (filters-more inc1? inc2? inc3? a-min? 
                                             a-max? b-min? b-max? c-min? c-max?)
                         (let-values ([(a-min? a-max?)  (if inc1? 
                                                            (values a-min? a-max?) 
                                                            (values a-max? a-min?))]
                                      [(b-min? b-max?)  (if inc2? 
                                                            (values b-min? b-max?) 
                                                            (values b-max? b-min?))]
                                      [(c-min? c-max?)  (if inc3? 
                                                            (values c-min? c-max?) 
                                                            (values c-max? c-min?))])
                           (cond [a-min?  0]
                                 [b-min?  1]
                                 [a-max?  2]
                                 [b-max?  3]
                                 [c-min?  4]
                                 [c-max?  5]
                                 [else    6])))
                       
                       (: bar : Any → Any)
                       (define (bar s)
                         (match s
                           [`(and) #f]
                           [`(and ,s) #f]
                           [`(and ,s ,sr ...) #f]
                           [`(or) #f]
                           [`(or ,s) #f]
                           [`(or ,s ,sr ...) #f]
                           [`(let* () ,s) #f]
                           [`(let* ((,(? symbol? x) ,s) ,b ...) ,t) #f]
                           [`(begin ,s ... ,t) #f]
                           [`(λ (,(? symbol? x) ...) ,t) #f]
                           [`(if ,s ,t1 ,t2) #f]
                           [`(let ,(list `[,(? symbol? x) ,s] ...) ,t) #f]
                           [`(recursive-contract ,c) #f]
                           [`(-> ,c ... ,d) #f]
                           [`(->i ([,(? symbol? x) ,c] ...) (,(? symbol? y) ,_ ,d)) #f]
                           [`(struct/c ,(? symbol? t) (,c ...)) #f]
                           [`(and/c ,c ...) #f]
                           [`(or/c ,c ...) #f]
                           [`(not/c ,c) #f]
                           [(list f xs ...) #f]
                           [#f #f]
                           [#t #f]
                           [(? number? x) #f]
                           [(? string? x) #f]
                           [(? symbol? x) 'reached])))
                     (require 'a)
                     foo
                     bar)
             (make-base-namespace))))