#lang typed/racket/shallow

(require typed/syntax/stx)

(let* ((stx #'(a b c)))
  (stx->list stx) ;; no shape-check
  (stx-car stx) ;; yes shape-check
  (stx-cdr stx) ;; yes shape-check
  (stx-map (ann values (-> Identifier Identifier)) (list #'a #'b)) ;; no shape-check
  (module-or-top-identifier=? #'ho #'la) ;; no shape-check
  (void))

