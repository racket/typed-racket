#lang typed/racket/base/shallow

;; Test the `ann` and `inst` forms

((ann car (All (A B) (-> (Pairof A B) A))) '(A B))
((inst car Symbol (Listof Symbol)) '(A B))
