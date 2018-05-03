#lang typed/racket

;; The precise type for a regexp should be the regexp itself:
(ann #rx"abc" #rx"abc")
(ann #px"abc" #px"abc")
(ann #rx"abc" '#rx"abc")
(ann #px"abc" '#px"abc")
;; The precise type for a byte-regexp should be the regexp itself:
(ann #rx#"abc" #rx#"abc")
(ann #px#"abc" #px#"abc")
(ann #rx#"abc" '#rx#"abc")
(ann #px#"abc" '#px#"abc")
;; Up-casting as Regexp should still work:
(ann #rx"abc" Regexp)
(ann (ann #rx"abc" #rx"abc") Regexp)
(ann (ann #rx"abc" '#rx"abc") Regexp)
(ann #px"abc" Regexp)
(ann (ann #px"abc" #px"abc") Regexp)
(ann (ann #px"abc" '#px"abc") Regexp)
(ann #px"abc" PRegexp)
(ann (ann #px"abc" #px"abc") PRegexp)
(ann (ann #px"abc" '#px"abc") PRegexp)
;; Up-casting as Byte-Regexp should still work:
(ann #rx#"abc" Byte-Regexp)
(ann (ann #rx#"abc" #rx#"abc") Byte-Regexp)
(ann (ann #rx#"abc" '#rx#"abc") Byte-Regexp)
(ann #px#"abc" Byte-Regexp)
(ann (ann #px#"abc" #px#"abc") Byte-Regexp)
(ann (ann #px#"abc" '#px#"abc") Byte-Regexp)
(ann #px#"abc" Byte-PRegexp)
(ann (ann #px#"abc" #px#"abc") Byte-PRegexp)
(ann (ann #px#"abc" '#px#"abc") Byte-PRegexp)

;; Check that the inferred type is still implicitly widened to (P)Regexp by
;; default, for backwards compatibility (previously, all regexps had the type
;; Regexp):
;; * Check that when passed as a #:∀ type, the inferred type is Regexp
(ann (let #:∀ (R) ([r : R #rx"abc"])
       (λ ([x : R]) r))
     (→ Regexp Regexp))
(ann (let #:∀ (R) ([r : R #px"abc"])
       (λ ([x : R]) r))
     (→ PRegexp PRegexp))
;; * Check that loops which rely on the first iteration having the wider
;;   (P)Regexp type still work:
(let loop : Void ([r #rx"abc"])
  (if (equal? r #rx"abc")
      (loop #rx"xyz")
      (void)))
(let loop : Void ([r #px"abc"])
  (if (equal? r #px"abc")
      (loop #px"xyz")
      (void)))

;; Check that the inferred type is still implicitly widened to Byte-(P)Regexp by
;; default, for backwards compatibility (previously, all regexps had the type
;; Regexp):
;; * Check that when passed as a #:∀ type, the inferred type is Byte-Regexp
(ann (let #:∀ (R) ([r : R #rx#"abc"])
       (λ ([x : R]) r))
     (→ Byte-Regexp Byte-Regexp))
(ann (let #:∀ (R) ([r : R #px#"abc"])
       (λ ([x : R]) r))
     (→ Byte-Regexp Byte-Regexp))
;; * Check that loops which rely on the first iteration having the wider
;;   Byte-(P)Regexp type still work:
(let loop : Void ([r #rx#"abc"])
  (if (equal? r #rx#"abc")
      (loop #rx#"xyz")
      (void)))
(let loop : Void ([r #px#"abc"])
  (if (equal? r #px#"abc")
      (loop #px#"xyz")
      (void)))

(define v (vector #rx"abc" #px"abc" #rx#"abc" #px#"abc"))
(define b1 (box #rx".*"))
(define b2 (box #px".*"))
(define b3 (box #rx#".*"))
(define b4 (box #px#".*"))
(define c1 (ann (box (ann #rx"abc" #rx"abc")) (Boxof #rx"abc")))
(define c2 (ann (box (ann #px"abc" #px"abc")) (Boxof #px"abc")))
(define c3 (ann (box (ann #rx#"abc" #rx#"abc")) (Boxof #rx#"abc")))
(define c4 (ann (box (ann #px#"abc" #px#"abc")) (Boxof #px#"abc")))

(vector-set! v 0 #rx".*")
(vector-set! v 1 #px".*")
(vector-set! v 2 #rx#".*")
(vector-set! v 3 #px#".*")
(set-box! b1 #rx"abc")
(set-box! b1 #px"abc") ;; Upcast #px to Regexp: b1 is a (Boxof Regexp)
(set-box! b2 #px"abc")
(set-box! b3 #rx#"abc")
(set-box! b3 #px#"abc") ;; Upcast #px# to Byte-Regexp: b3 is a (Boxof Regexp)
(set-box! b4 #px#"abc")
;; (set-box! c1 #rx".*") ouch, it would be nice if this worked
;; (set-box! c2 #px".*") ouch, it would be nice if this worked
;; (set-box! c3 #rx#".*") ouch, it would be nice if this worked
;; (set-box! c4 #px#".*") ouch, it would be nice if this worked
(set-box! c1 (ann #rx"abc" #rx"abc"))
(set-box! c2 (ann #px"abc" #px"abc"))
(set-box! c3 (ann #rx#"abc" #rx#"abc"))
(set-box! c4 (ann #px#"abc" #px#"abc"))


(ann (let ([x : #rx"abc" #rx"abc"])
       (if ((make-predicate #rx"abc") x)
           x
           0))
     #rx"abc")

(ann (let ([x : #px"abc" #px"abc"])
       (if ((make-predicate #px"abc") x)
           x
           0))
     #px"abc")

(ann (let ([x : #rx#"abc" #rx#"abc"])
       (if ((make-predicate #rx#"abc") x)
           x
           0))
     #rx#"abc")

(ann (let ([x : #px#"abc" #px#"abc"])
       (if ((make-predicate #px#"abc") x)
           x
           0))
     #px#"abc")