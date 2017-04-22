#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         racket/match
         (rep core-rep)
         (types abbrev tc-result prop-ops)
         (r:infer infer))

(provide/cond-contract [intersect-tc-results (-> (cons full-tc-results/c (listof full-tc-results/c))
                                                 full-tc-results/c)]
                       [tc-res-intersect (-> full-tc-results/c full-tc-results/c full-tc-results/c)])


(define (intersect-tc-results ress)
  (match ress
    [(cons res ress)
     (for/fold ([acc res])
               ([res (in-list ress)])
       (tc-res-intersect acc res))]))

(define (tc-res-intersect res1 res2)
  (match* (res1 res2)
    [(r r) r]
    [((tc-any-results: _) _) (tc-any-results -tt)]
    [(_ (tc-any-results: _)) (tc-any-results -tt)]
    [((tc-results: ts1 (list (PropSet: ps1+ ps1-) ...) os1)
      (tc-results: ts2 (list (PropSet: ps2+ ps2-) ...) os2))
     #:when (= (length ts1) (length ts2))
     (ret (for/list ([t1 (in-list ts1)]
                     [t2 (in-list ts2)])
            (intersect t1 t2))
          (for/list ([p1+ (in-list ps1+)]
                     [p2+ (in-list ps2+)]
                     [p1- (in-list ps1-)]
                     [p2- (in-list ps2-)])
            (-PS (-and p1+ p2+)
                 (-and p1- p2-)))
          (for/list ([o1 (in-list os1)]
                     [o2 (in-list os2)])
            (if (equal? o1 o2) o1 -empty-obj)))]
    ;; else punt
    [(_ _) (tc-any-results -tt)]))