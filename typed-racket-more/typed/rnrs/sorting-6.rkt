#lang s-exp typed-racket/base-env/extra-env-lang

(require rnrs/sorting-6)

(type-environment
 [list-sort (-poly (a) (-> (-> a a Univ) (-lst a) (-lst a)))]
 [vector-sort (-poly (a) (-> (-> a a Univ) (-vec a) (-vec a)))]
 [vector-sort! (-poly (a) (-> (-> a a Univ) (-vec a) -Void))])
