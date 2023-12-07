#lang typed/racket/base

(require racket/flonum)

(lambda ([xs : FlVector])
  (ann  (#%app reverse
               (let-values ([(d) null]
                            ((vec len)
                             (#%app values xs (#%app flvector-length xs))))
                 (#%app
                  (letrec-values (((for-loop)
                                   (lambda (fold-var pos)
                                     (let-values ()
                                       (if (#%expression (#%app < pos len))
                                           (let-values (((x)
                                                         (#%app
                                                          flvector-ref
                                                          vec
                                                          pos)))
                                             (if (#%expression '#t)
                                                 (let-values (((fold-var)
                                                               (let-values ()
                                                                 (#%app
                                                                  cons
                                                                  x
                                                                  fold-var))))
                                                   (if (if '#t
                                                           (#%expression (#%app not '#f))
                                                           '#f)
                                                       (#%app
                                                        for-loop
                                                        fold-var
                                                        (#%app + '1 pos))
                                                       fold-var))
                                                 fold-var))
                                           fold-var)))))
                    for-loop)
                  d
                  (#%datum . 0)))) 
       (Listof Flonum)))

