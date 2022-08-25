#lang typed/racket/base/shallow

;; Expand and look for shape-check / lack of.
;; See comments below.

(let ((x0 : (List) '()))
  (cdr x0) ;; yes shape-check
  (cddr x0)) ;; yes shape-check

(let ((x1 : (List Symbol) '(a)))
  (cdr x1) ;; no shape-check
  (cddr x1)) ;; yes shape-check

(let ((x2 : (List Symbol Symbol) '(a b)))
  (cdr x2) ;; no shape-check
  (cddr x2) ;; no shape-check
  (cdddr x2)) ;; yes shape-check after (cdr (unsafe-cdr (unsafe-cdr _)))

(let ((x3 : (List Symbol Symbol Symbol) '(a b c)))
  (cdr x3) ;; no shape-check
  (cddr x3) ;; no shape-check
  (cdddr x3) ;; no shape-check
  (cddddr x3)) ;; yes shape-check after (cdr (unsafe-cdr (unsafe-cdr _)))

(let ((x4 : (List Symbol Symbol Symbol Symbol) '(a b c d)))
  (cdr x4) ;; no shape-check
  (cddr x4) ;; no shape-check
  (cdddr x4) ;; no shape-check
  (cddddr x4)) ;; no shape-check

(let ((x5 : (Pairof Symbol Null) '(a . ())))
  (cdr x5)) ;; no shape-check
